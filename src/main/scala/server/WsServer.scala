package server

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl._
import akka.stream.{ActorMaterializer, OverflowStrategy}
import server.actors.ReplActor
import server.service._

import scala.concurrent.{ExecutionContext, Future}


object WsServer {

  def auth(message: String, userService: UserService)
          (implicit ec: ExecutionContext): Future[Protocol] =
    Protocol.decodeRequestJson(message) match {
      case Left(error) =>
        Future.successful(InternalError(error = error.toString))
      case Right(envelope) =>
        envelope match {
          case LoginRequest(_, username, password) =>
            userService.verifyUser(username, password).map {
              case Some(user) =>
                SuccessfulLoginResponse(user_type = user.$type)
              case None =>
                FailedLoginResponse()
            }
        }
    }

  def route(url: String, userService: UserService)
           (implicit ec: ExecutionContext, mat: ActorMaterializer, system: ActorSystem): Route = {
    val actor = system.actorOf(ReplActor.props())

    val in =
      Flow[Message]
        .mapConcat {
          case tm: TextMessage =>
            tm.getStrictText :: Nil
          case bm: BinaryMessage =>
            // ignore binary messages but drain content to avoid the stream being clogged
            // This will lose (ignore) messages not received in one chunk (which is
            // unlikely because chat messages are small) but absolutely possible
            // FIXME: need to handle TextMessage.Streamed as well.
            bm.dataStream.runWith(Sink.ignore)
            Nil
        }
        .mapAsync(42)(auth(_, userService))
        .to(Sink.actorRef(actor, End))

    val out =
      Source
        .actorRef(bufferSize = 42, overflowStrategy = OverflowStrategy.fail)
        .mapMaterializedValue(actor ! WsHandler(_))

    val wsFlow = Flow.fromSinkAndSource(in, out)

    path(url) {
      get {
        handleWebSocketMessages(wsFlow)
      }
    }
  }

}