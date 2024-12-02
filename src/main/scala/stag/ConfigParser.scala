package stag

import scala.io.Source
import scala.util.Try

object ConfigParser {

  case class Config(values: Map[String,String]) {
    def getString(key: String): Option[String] = values.get(key)
    def getInt(key: String): Option[Int] = values.get(key).map(_.toInt)
    def getBoolean(key: String): Option[Boolean] = values.get(key).map(_.toBoolean)
  }

  def parseConfigFile(filename: String): Try[Config] = {
    Try {
      val source = Source.fromResource(filename)
      val lines = source.getLines().filterNot(line => line.trim.isEmpty || line.trim.startsWith("#") || line.startsWith("["))
      val configMap = lines.collect {
        case line if line.contains(":") =>
          val Array(key, value) = line.split(":", 2).map(_.trim)
          key -> value
      }.toMap
      source.close()
      Config(configMap)
    }
  }
}