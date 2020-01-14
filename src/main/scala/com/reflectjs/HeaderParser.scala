package com.reflectjs

import scala.collection.mutable

class HeaderParseError(message: String) extends Exception

class HeaderParser() {
  private var data = ""
  private val headerPattern = """([a-zA-Z\-]+)\s*:\s*([^\r]+)\r\n""".r
  private val requestPattern = """^([A-Z]+) ([^\s]+) HTTP/(\d.\d)\r\n""".r
  private val bodyPattern = """\r\n\r\n""".r
  val headers = new mutable.HashMap[String, String]()
  var requestMethod = ""
  var requestPath = ""
  var httpVersion = 1.0
  var body = ""
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
    requestPattern.findFirstMatchIn(data) match {
      case Some(m) =>
        requestMethod = m.group(1)
        requestPath = m.group(2)
        httpVersion = m.group(3).toDouble
      case None =>
        throw new HeaderParseError("Failed to parse http request first line")
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
    headers.map(_ match {
      case (k, v) => s"${k}: ${v}"
    }).mkString("\r\n") + "\r\n\r\n" + body
  }

  def toByteArray: (Array[Byte], Int) = {
    val arr = toString().map(_.toByte).toArray
    (arr, arr.length)
  }
}
