package server

import akka.actor.ActorSystem
import akka.http.scaladsl.testkit.{ScalatestRouteTest, WSProbe}
import akka.stream.ActorMaterializer
import io.circe.generic.auto._
import io.circe.syntax._
import server.service.{SimpleUserService, User}

class WsServerSpec extends BaseSpec with ScalatestRouteTest {

  val path = "path"

  it should "do auth" in {

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    val username = "user"
    val password = "pass"
    val user_type = "admin"
    val user = User(user_type, username, password)

    val userService = new SimpleUserService()
    userService.addUser(user)

    val route = WsServer.route(path, userService)

    val wsClient = WSProbe()

    val failedLogin = FailedLogin().asJson.toString
    val successLogin = SuccessfulLogin(user_type = user_type)

    WS(s"/$path", wsClient.flow) ~> route ~>
      check {
        isWebSocketUpgrade shouldEqual true

        wsClient.sendMessage(LoginRequest(username = "a", password =  "la").asJson.toString)
        wsClient.expectMessage(failedLogin)

        wsClient.sendMessage(LoginRequest(username = username, password =  password).asJson.toString)
        wsClient.expectMessage(successLogin.asJson.toString)
      }

  }

  it should "do actor" in {

      implicit val system = ActorSystem()
      implicit val materializer = ActorMaterializer()

      val username = "user"
      val password = "pass"
      val user_type = "admin"
      val user = User(user_type, username, password)

      val userService = new SimpleUserService()
      userService.addUser(user)

      val route = WsServer.route(path, userService)

      val wsClient = WSProbe()

      val successLogin = SuccessfulLogin(user_type = user_type)

      WS(s"/$path", wsClient.flow) ~> route ~>
        check {
          isWebSocketUpgrade shouldEqual true

          wsClient.sendMessage(LoginRequest(username = username, password =  password).asJson.toString)
          wsClient.expectMessage(successLogin.asJson.toString)
        }
  }

//  it should "work" in {
//        implicit val system = ActorSystem()
//        implicit val materializer = ActorMaterializer()
//
//        val path = "path"
//        val user = "user"
//        val pass = "pass"
//
//        val authService = new SimpleAuthService(Map(user -> pass))
//        val credentials = BasicHttpCredentials(user, pass)
//
//        val route = Evolution.route(path, authService)
//
//        val wsClient = WSProbe()
//
//        WS(s"/$path", wsClient.flow) ~> route ~>
//          check {
//            isWebSocketUpgrade shouldEqual true
//
//            wsClient.sendMessage("Peter".asRequestJson)
//            wsClient.expectMessage(Envelope.encodeResponseJson("Peter"))
//
//            wsClient.sendMessage(BinaryMessage(ByteString("abcdef")))
//            wsClient.expectNoMessage(100.millis)
//
//            wsClient.sendMessage("John".asRequestJson)
//            wsClient.expectMessage(Envelope.encodeResponseJson("John"))
//
//            wsClient.sendCompletion()
//            wsClient.expectCompletion()
//
//            wsClient.sendMessage("John")
//            wsClient.expectNoMessage()
//          }
//
//  }

}
