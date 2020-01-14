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

    val server = new Socket("www.bing.com", 80)
    val serverIn = server.getInputStream()
    val serverOut = server.getOutputStream()

    new ClientThread(serverOut, clientIn).start()

    var bytesRead = serverIn.read(reply)
    while (bytesRead != -1) {
      clientOut.write(reply, 0, bytesRead)
      bytesRead = serverIn.read(reply)
    }
  }
}

/**
 * Thread that writes data from the client to the server
 * @param serverOut output stream to the server
 * @param clientIn input stream from the client
 */
class ClientThread(serverOut: OutputStream, clientIn: InputStream) extends Thread {
  override def run(): Unit = {
    var req = new Array[Byte](4096)
    var bytesRead = 0
    bytesRead = clientIn.read(req)
    val parser = new HeaderParser
    while (bytesRead != -1) {
      try {
        parser.parse(req, bytesRead)
        parser.headers.foreach(println(_))
        println(parser.body)
        parser.headers("Host") = "www.bing.com"
        parser.toByteArray match {
          case (arr, len) =>
            req = arr
            bytesRead = len
        }
        serverOut.write(req, 0, bytesRead)
        bytesRead = clientIn.read(req)
      }
    }
  }
}
