package com.reflectjs

import scala.collection.mutable

class HeaderParseError(message: String) extends Exception

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
      protocol = Option(m.group(1)).getOrElse("http://")
      host = m.group(2)
      port = Option(m.group(4)).getOrElse("80").toInt
      absolutePath = Option(m.group(6)).getOrElse("/")
      query = Option(m.group(7)).getOrElse("")
    case None =>
      throw new HeaderParseError("Failed to parse http path")
  }
}

class HeaderParser(transactionType: Transaction.Type) {
  private var data = ""
  private val headerPattern = """([a-zA-Z\-]+)\s*:\s*([^\r]+)\r\n""".r
  private val requestPattern = """^([A-Z]+) ([^\s]+) HTTP/(\d\.\d)\r\n""".r
  private val responsePattern = """HTTP/(\d\.\d) (\d+) ([A-Z]+)\r\n""".r
  private val bodyPattern = """\r\n\r\n""".r

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

    data = rawData.slice(0, length).map(_.toChar).mkString
    //println(data)
    for (patternMatch <- headerPattern.findAllMatchIn(data)) {
      headers.addOne(patternMatch.group(1), patternMatch.group(2))
    }
    transactionType match {
      case Transaction.Request =>
        requestPattern.findFirstMatchIn(data) match {
          case Some(m) =>
            requestMethod = m.group(1)
            requestPath = m.group(2)
            httpVersion = m.group(3).toDouble
          case None =>
            throw new HeaderParseError("Failed to parse http request first line")
        }
      case Transaction.Response =>
        responsePattern.findFirstMatchIn(data) match {
          case Some(m) =>
            responseCode = m.group(2).toInt
            httpVersion = m.group(1).toDouble
            responseOk = m.group(3)
          case None =>
            throw new HeaderParseError("Failed to parse http response first line")
        }
    }
    bodyPattern.findFirstMatchIn(data) match {
      case Some(m) =>
        body = data.slice(m.start, try {headers("Content-Length").toInt} catch {
          case _: NoSuchElementException => data.length
          case _: NumberFormatException => data.length
        })
    }
  }

  override def toString: String = {
    s"${requestMethod} ${requestPath} HTTP/${httpVersion.toString}\r\n" +
    headers.map {
      case (k, v) => s"${k}: ${v}"
    }.mkString("\r\n") + "\r\n\r\n" + body
  }

  def toByteArray: (Array[Byte], Int) = {
    val arr = toString().map(_.toByte).toArray
    (arr, arr.length)
  }
}
