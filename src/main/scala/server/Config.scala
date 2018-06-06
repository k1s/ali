package server

import com.typesafe.config.ConfigFactory

case class Config(port: Int,
                  wsUrl: String)

object Config {

  def read: Config = pureconfig.loadConfigOrThrow[Config](ConfigFactory.load.getConfig("evolution"))

}