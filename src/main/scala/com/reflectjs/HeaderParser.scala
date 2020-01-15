package com.reflectjs

import scala.collection.mutable
class HeaderParseError(message: String) extends Exception(message)

object Transaction extends Enumeration {
  type Type = Value
  val Request, Response = Value
}

case class Path(fullPath: String) {
  private val pathPattern = """^(https?://)([a-zA-Z\-\.\d]+)(:(\d+))?((/[^\s^\?]*)(\?[^\s]*)?)?$""".r
  var protocol = "http://"
  var host = ""
  var absolutePath = "/"
  var query = ""
  var port = 80
  pathPattern.findFirstMatchIn(fullPath) match {
    case Some(m) =>
      protocol = Option(m.group(1)).getOrElse("http://")
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
}

class HeaderParser(transactionType: Transaction.Type) {
  private var data = ""
  private val headerPattern = """\n([a-zA-Z\-]+)\s*:\s*([^\r]+)\r\n""".r
  private val requestPattern = """^([A-Z]+) ([^\s]+) HTTP/(\d\.\d)\r\n""".r
  private val responsePattern = """HTTP/(\d\.\d) (\d+) ([^\r]+)\r\n""".r

  val headers = new mutable.HashMap[String, String]()
  var requestMethod = ""
  var requestPath = ""
  var httpVersion = 1.0
  var body = ""
  var responseCode = 200
  var responseOk = "OK"
  def parse(rawData: Array[Byte], length: Int): Unit = {
    headers.clear()
    requestMethod = ""
    requestPath = ""
    httpVersion = 1.0
    body = ""

    data = new String(rawData.slice(0, length), "UTF-8")
    val split = data.split("\r\n\r\n")
    var dataHeaders = data

    if (split.length >= 2) {
      dataHeaders = split(0)
      body = split.slice(1, split.length).mkString("\r\n\r\n")
    }

    for (patternMatch <- headerPattern.findAllMatchIn(data)) {
      headers.addOne(patternMatch.group(1), patternMatch.group(2))
    }
    transactionType match {
      case Transaction.Request =>
        requestPattern.findFirstMatchIn(dataHeaders) match {
          case Some(m) =>
            requestMethod = m.group(1)
            requestPath = m.group(2)
            httpVersion = m.group(3).toDouble
          case None =>
            println(data.slice(0, 100))
            throw new HeaderParseError("Failed to parse http request first line")
        }
      case Transaction.Response =>
        responsePattern.findFirstMatchIn(dataHeaders) match {
          case Some(m) =>
            responseCode = m.group(2).toInt
            httpVersion = m.group(1).toDouble
            responseOk = m.group(3)
          case None =>
            println(data.slice(0, 100))
            throw new HeaderParseError("Failed to parse http response first line")
        }
    }
  }

  override def toString: String = {
    //headers("Content-Length") = body.length.toString

    (transactionType match {
      case Transaction.Request => s"${requestMethod} ${requestPath} HTTP/${httpVersion.toString}\r\n"
      case Transaction.Response => s"HTTP/${httpVersion} ${responseCode} ${responseOk}\r\n"
    }) +
    headers.map {
      case (k, v) => s"${k}: ${v}"
    }.mkString("\r\n") + "\r\n\r\n" + body //(if (body != "") { "\r\n\r\n" + body } else "" )
  }

  def toByteArray: (Array[Byte], Int) = {
    val str = toString
    val arr = str.map(_.toByte).toArray
    (arr, str.length)
  }
}
