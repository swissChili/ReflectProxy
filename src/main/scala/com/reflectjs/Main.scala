package com.reflectjs

import java.net._
import java.io._

import javax.net.ssl.SSLSocketFactory

import scala.collection.mutable.ArrayBuffer


object Main {
  def main(args: Array[String]): Unit = {
    val localPort = 1234
    val server = new ServerSocket(localPort)
    while (true) {
      new ProxyThread(server.accept()).start()
    }
  }
}

class ProxyThread(client: Socket) extends Thread("Proxy Thread") {
  override def run(): Unit = {
    val clientOut = client.getOutputStream
    val clientIn = new BufferedReader(
      new InputStreamReader(client.getInputStream)
    )
    var server: Socket = null // SSLSocketFactory.getDefault.createSocket("www.google.com", 443)
    var serverOut: OutputStream = null // server.getOutputStream
    var serverIn: InputStream = null // server.getInputStream
    var host = ""

    var knowPath = false
    var inBody = false
    var line = clientIn.readLine()
    while (line != null) {
      if (!knowPath) {
        val request = RequestLine(line)
        val path = Path(request.path.slice(1, request.path.length))
        println(s"The path is ${path.host} : ${path.port}")
        host = path.host
        server = SSLSocketFactory.getDefault.createSocket(path.host, 443)
        serverOut = server.getOutputStream
        serverIn = server.getInputStream

        request.path = path.absolutePath + path.query
        line = request.toString

        new TransmitterThread(clientOut, serverIn).start()
        knowPath = true
      }

      if (line == "") inBody = true

      if (!inBody) {
        try {
          val header = Header(line)
          if (header.key == "Host") header.value = host
          line = header.toString
        } catch {
          case _: HeaderParseError => // bruh
        }
      }

      serverOut.write((line + "\r\n").toCharArray.map(_.toByte), 0, line.length + 2)
      serverOut.flush()
      println(s"Wrote $line")
      line = clientIn.readLine()
    }
    println("client done")
  }
}

class TransmitterThread(out: OutputStream, in: InputStream) extends Thread("Transmitter Thread") {
  override def run(): Unit = {
    val bytes = new Array[Byte](4096)

    var bytesRead = in.read(bytes)
    while (bytesRead != -1) {
      println(s"Writing $bytesRead")
      out.write(bytes, 0, bytesRead)
      out.flush()
      bytesRead = in.read(bytes)
    }
    println("done?")
  }
}
