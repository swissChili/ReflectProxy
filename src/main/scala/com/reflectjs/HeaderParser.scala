package com.reflectjs

import scala.collection.mutable
import java.util.logging.{Logger, Level}
class HeaderParseError(message: String) extends Exception(message)

object Transaction extends Enumeration {
  type Type = Value
  val Request, Response = Value
}

case class Path(fullPath: String) {
  private val pathPattern = """^(https?://)?([a-zA-Z\-\.\d]+)(:(\d+))?((/[^\s^\?]*)(\?[^\s]*)?)?$""".r
  var protocol = "http://"
  var host = ""
  var absolutePath = "/"
  var query = ""
  var port = 80
  pathPattern.findFirstMatchIn(fullPath) match {
    case Some(m) =>
      protocol = Option(m.group(1)).getOrElse("https://")
      host = m.group(2)
      port = Option(m.group(4)).getOrElse(protocol match {
        case "http://" => "80"
        case "https://" => "443"
      }).toInt
      absolutePath = Option(m.group(6)).getOrElse("/")
      query = Option(m.group(7)).getOrElse("")
    case None =>
      throw new HeaderParseError("Failed to parse http path")
  }

  override def toString: String = s"$protocol$host:$port$absolutePath$query"
}

case class RequestLine(line: String)(implicit logger: Logger) {
  private val reqPattern = """^([A-Z]+) ([^\s]+) HTTP/(\d\.\d)""".r
  var method = "GET"
  var path: String = _
  var httpVersion = 1.0
  reqPattern.findFirstMatchIn(line) match {
    case Some(m) =>
      method = m.group(1)
      path = m.group(2)
      httpVersion = m.group(3).toDouble
    case None =>
      logger.log(Level.INFO, line)
      throw new HeaderParseError("Could not parse request line")
  }

  override def toString: String = s"$method $path HTTP/$httpVersion"
}

case class Header(line: String) {
  private val headerPattern = """^([^:^\s]+)\s*:\s*(.*)""".r
  var key: String = _
  var value: String = _
  headerPattern.findFirstMatchIn(line) match {
    case Some(m) =>
      key = m.group(1)
      value = m.group(2)
    case None =>
      throw new HeaderParseError("Could not parse header line")
  }

  override def toString: String = s"$key: $value"
}
