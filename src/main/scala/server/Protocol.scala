package server

import akka.actor.ActorRef
import akka.http.scaladsl.model.ws.TextMessage
import io.circe
import io.circe.generic.auto._
import io.circe.{Decoder, Encoder, parser}
import io.circe.syntax._

sealed trait Protocol {

  def $type: String

}

case class InternalError($type: String = "error", error: String) extends Protocol

case class LoginRequest($type: String = "login", username: String, password: String) extends Protocol
case class FailedLogin($type: String = "login_failed") extends Protocol
case class SuccessfulLogin($type: String = "login_successful", user_type: String) extends Protocol

case class Ping($type: String = "ping", seq: Int) extends Protocol
case class Pong($type: String = "pong", seq: Int) extends Protocol

case class End($type: String = "end") extends Protocol

object Protocol {

  def response(envelope: Protocol): TextMessage = {
    import Protocol.encodeEnvelope
    TextMessage(envelope.asJson.toString)
  }

  implicit val encodeEnvelope: Encoder[Protocol] = Encoder.instance {
    case f: FailedLogin => f.asJson
    case s: SuccessfulLogin => s.asJson
  }

  implicit val decodeEnvelope: Decoder[Protocol] =
    for {
      visitorType <- Decoder[String].prepare(_.downField("$type"))
      value <- visitorType match {
        case "login" => Decoder[LoginRequest]
        case "ping" => Decoder[Ping]
        case other => Decoder.failedWithMessage(s"invalid type: $other")
      }
    } yield value

  def decodeRequestJson(json: String): Either[circe.Error, Protocol] =
    parser.decode[Protocol](json)

  case class Response(response: String)

  def encodeResponseJson(response: String): String =
    Response(response).asJson.toString

}

case class Subscriber(actor: ActorRef)
