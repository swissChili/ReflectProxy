package com.reflectjs

import java.net._
import java.io.{InputStream, OutputStream}

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

    var bytesRead = clientIn.read(reply)
    while (bytesRead != -1) {
      parser.parse(reply, bytesRead)
      // Slice the / off /www.website.com
      val serverPath = Path(parser.requestPath.slice(1, parser.requestPath.length))
      val server = new Socket(serverPath.host, serverPath.port)
      val serverIn = server.getInputStream
      val serverOut = server.getOutputStream

      new ServerThread(clientOut, serverIn).start()

      parser.headers("Host") = serverPath.host
      parser.requestPath = s"${serverPath.absolutePath}${serverPath.query}"

      val (sendBytes, len) = parser.toByteArray
      serverOut.write(sendBytes, 0, len)
      bytesRead = clientIn.read(reply)
    }
  }
}

class ServerThread(clientOut: OutputStream, serverIn: InputStream) extends Thread {
  override def run(): Unit = {
    val req = new Array[Byte](4096)
    var bytesRead = 0
    bytesRead = serverIn.read(req)
    //val parser = new HeaderParser(Transaction.Request)
    while (bytesRead != -1) {
      try {
        clientOut.write(req, 0, bytesRead)
        bytesRead = serverIn.read(req)
      }
    }
  }
}
