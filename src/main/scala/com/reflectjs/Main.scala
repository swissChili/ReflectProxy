package com.reflectjs

import java.net._
import javax.net.ssl._
import java.io.{InputStream, OutputStream}
//import requests._

object Main {
  def main(args: Array[String]): Unit = {
    val localPort = 1234
    val server = new ServerSocket(localPort)
    while (true) {
      new ThreadProxy(server.accept()).start()
    }
  }
}

class ThreadProxy(client: Socket) extends Thread {
  override def run(): Unit = {
    val request = new Array[Byte](4096)

    val clientIn = client.getInputStream
    val clientOut = client.getOutputStream

    val parser = new HeaderParser(Transaction.Request)

    var bytesRead = clientIn.read(request)
    while (bytesRead != -1) {
      parser.parse(request, bytesRead)
      // Slice the / off /www.website.com
      val serverPath = Path(parser.requestPath.slice(1, parser.requestPath.length))
      println(s"Port is ${serverPath.port}")
      val server = serverPath.protocol match {
        case "https://" => SSLSocketFactory.getDefault.createSocket(serverPath.host, serverPath.port)
        case "http://" => new Socket(serverPath.host, serverPath.port)
      }
      val serverIn = server.getInputStream
      val serverOut = server.getOutputStream

      new ServerThread(clientOut, serverIn, server).start()

      parser.headers("Host") = serverPath.host
      parser.requestPath = s"${serverPath.absolutePath}${serverPath.query}"

      val (arr, len) = parser.toByteArray

      println("~~ Request ~~")
      println(parser.toString)
      println("~~ /Request ~~")
      println(s"len = $len")

      serverOut.write(arr, 0, len)
      bytesRead = clientIn.read(request)
    }
  }
}

class ServerThread(clientOut: OutputStream, serverIn: InputStream, server: Socket) extends Thread {
  override def run(): Unit = {
    val req = new Array[Byte](4096)
    val parser = new HeaderParser(Transaction.Response)

    println(s"Server Thread ${serverIn.available()}")

    var bytesRead = serverIn.read(req)
    while (bytesRead != -1) {
      println(s"Read $bytesRead bytes")
      try {
        parser.parse(req, bytesRead)
        println(s"Res = ${parser.responseOk}")
        parser.headers.remove("X-Frame-Options")
        if (parser.headers.contains("Location")) {
          parser.headers("Location") = "localhost:1234/" + parser.headers("Location")
          println(parser.headers("Location"))
        }
        val contentEncoding = parser.headers.getOrElse("Content-Encoding", "identity")
        // parser.headers("Content-Encoding") = "gzi"
        println(s"~~ Encoding = $contentEncoding ~~")
        parser.headers.foreach{case (k, v) => println(s"$k: $v")}
        val resStr = parser.toString
        val (bytes, len) = parser.toByteArray
        clientOut.write(bytes, 0, len)
        println(s"~~ Wrote Modified $len")
      } catch {
        case _: HeaderParseError =>
          try {
            clientOut.write(req, 0, bytesRead)
            println(s"~~ Wrote $bytesRead")
          } catch {
            case _: SocketException =>
              println("Looks like we're done.")
          }
      }
      println(s"~~ Finished reading $bytesRead")
      bytesRead = serverIn.read(req)
    }

    println("Goodbye!")
    server.close()
  }
}
