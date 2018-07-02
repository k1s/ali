package server

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl._
import akka.stream.{ActorMaterializer, OverflowStrategy}
import server.actors.WsActor
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
                SuccessfulLogin(user_type = user.$type)
              case None =>
                FailedLogin()
            }
        }
    }

  def route(url: String, userService: UserService)
           (implicit ec: ExecutionContext, mat: ActorMaterializer, system: ActorSystem): Route = {
    val wsActor = system.actorOf(WsActor.props())

    val sink =
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
        .to(Sink.actorRef(wsActor, End))

    val source =
      Source
        .actorRef(bufferSize = 42, overflowStrategy = OverflowStrategy.fail)
        .mapMaterializedValue(subscriber => wsActor ! Subscriber(subscriber))

    val wsFlow = Flow.fromSinkAndSource(sink, source)

    path(url) {
      get {
        handleWebSocketMessages(wsFlow)
      }
    }
  }

}