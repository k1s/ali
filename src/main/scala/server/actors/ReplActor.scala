package server.actors

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.event.LoggingReceive
import server._

class ReplActor() extends Actor with ActorLogging {

  private[this] var wsHandle: Option[ActorRef] = None

  def receive: Receive = LoggingReceive {
    case WsHandler(wsHandler) =>
      log.warning(s"handler $wsHandler")
      wsHandle = Some(wsHandler)
    case msg: Protocol =>
      msg match {
        case Ping(_, seq) =>
          Pong(seq = seq)
        case other =>
          log.warning(s"Other message: $other")

          wsHandle.get ! Protocol.response(other)
      }

    case other =>
      log.warning(s"Unmatched message: $other")
  }

}

object ReplActor {

  def props(): Props = Props(new ReplActor())

  def replActor(system: ActorSystem): ActorRef = system.actorOf(ReplActor.props())

}
