package server.service

import scala.concurrent.{ExecutionContext, Future}

trait UserService {

  def addUser(user: User)(implicit ec: ExecutionContext): Future[Option[User]]

  def getUser(username: String)(implicit ec: ExecutionContext): Future[Option[User]]

  def verifyUser(username: String, password: String)(implicit ec: ExecutionContext): Future[Option[User]]

}

class SimpleUserService extends UserService {

  private[this] val userInfo = new java.util.concurrent.ConcurrentHashMap[String, User]()

  override def addUser(user: User)(implicit ec: ExecutionContext): Future[Option[User]] =
    Future.successful {
      if (!userInfo.contains(user.username))
        Option(userInfo.put(user.username, user))
      else
        None
    }

  override def getUser(username: String)(implicit ec: ExecutionContext): Future[Option[User]] =
    Future.successful {
      Option(userInfo.get(username))
    }

  override def verifyUser(username: String,
                          password: String)(implicit ec: ExecutionContext): Future[Option[User]] =
    getUser(username) map {
      case Some(user@User(_, _, password_)) if password_ == password =>
        Some(user)
      case _ => None
    }

}

case class User($type: String, username: String, password: String)
