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
    val reply = new Array[Byte](4096)

    val clientIn = client.getInputStream
    val clientOut = client.getOutputStream

    val parser = new HeaderParser(Transaction.Request)
    val resParser = new HeaderParser(Transaction.Response)

    var bytesRead = clientIn.read(reply)
    while (bytesRead != -1) {
      parser.parse(reply, bytesRead)
      // Slice the / off /www.website.com
      val serverPath = Path(parser.requestPath.slice(1, parser.requestPath.length))
      println(s"Port is ${serverPath.port}")
      val server = serverPath.protocol match {
        case "https://" => SSLSocketFactory.getDefault.createSocket(serverPath.host, serverPath.port)
        case "http://" => new Socket(serverPath.host, serverPath.port)
      }
      val serverIn = server.getInputStream
      val serverOut = server.getOutputStream

      new ServerThread(clientOut, serverIn).start()

      parser.headers("Host") = serverPath.host
      parser.requestPath = s"${serverPath.absolutePath}${serverPath.query}"

      val (arr, len) = parser.toByteArray

      serverOut.write(arr, 0, len)
      bytesRead = clientIn.read(reply)
    }
  }
}

class ServerThread(clientOut: OutputStream, serverIn: InputStream) extends Thread {
  override def run(): Unit = {
    val req = new Array[Byte](4096)
    val parser = new HeaderParser(Transaction.Response)

    var bytesRead = serverIn.read(req)
    while (bytesRead != -1) {
      try {
        parser.parse(req, bytesRead)
        println(s"Res = ${parser.responseOk}")
        parser.headers.foreach(println(_))
        parser.headers.remove("X-Frame-Options")
        if (parser.headers.contains("Location")) {
          parser.headers("Location") = "localhost:1234/" + parser.headers("Location")
          println(parser.headers("Location"))
        }
        val (bytes, len) = parser.toByteArray
        clientOut.write(bytes, 0, len)
      } catch {
        case _: HeaderParseError =>
          try {
            clientOut.write(req, 0, bytesRead)
          } catch {
            case _: SocketException =>
              println("Looks like we're done.")
          }
      }
      bytesRead = serverIn.read(req)
    }
  }
}
