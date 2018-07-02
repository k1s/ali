package server.actors

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.event.LoggingReceive
import server._

import scala.collection.mutable

class WsActor() extends Actor with ActorLogging {

  def receive: Receive = active(Map.empty[String, ActorRef])

  def active(subscribers: Map[String, ActorRef]): Receive = LoggingReceive {
    case Subscriber(subscriber) =>
      log.warning(s"SUBSCRIBER $subscriber")

//      context.watch()
      context become active(subscribers + ("" -> subscriber))
//      anotherActor = Some(wsHandler)
    case msg: Protocol =>
      msg match {
        case Ping(_, seq) =>
          Pong(seq = seq)
        case other =>
          log.warning(s"OTHER: $other")

          subscribers("") ! Protocol.response(other)
      }

    case other =>
      log.warning(s"UNMATCHED: $other")
  }

}

object WsActor {

  def props(): Props = Props(new WsActor())

  def replActor(system: ActorSystem): ActorRef = system.actorOf(WsActor.props())

}
