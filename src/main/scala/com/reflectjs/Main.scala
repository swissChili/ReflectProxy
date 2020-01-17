package com.reflectjs

import java.net._
import java.io._
import java.util.logging.{ConsoleHandler, Level, Logger}

import javax.net.ssl.SSLSocketFactory
import rawhttp.core.{RawHttp, RawHttpHeaders, RawHttpResponse}

object Main {
  implicit val logger: Logger = Logger.getLogger("Logger")
  logger.setUseParentHandlers(false)
  val handler = new ConsoleHandler()
  val formatter = new LogFormatter()
  handler.setFormatter(formatter)
  logger.addHandler(handler)

  implicit val analytics: Analytics = sys.env.get("PROXY_DISABLE_SQL") match {
    case Some(x) => new UselessAnalytics
    case None => new MySQLAnalytics(
      sys.env.getOrElse("MYSQL_USER", "test"),
      sys.env.getOrElse("MYSQL_PASS", "password")
    )
  }

  def main(args: Array[String]): Unit = {
    val localPort = sys.env.getOrElse("PORT", "1234").toInt
    val server = new ServerSocket(localPort)
    logger.log(Level.INFO, s"Serving on :$localPort")
    while (true) {
      new ProxyThread(server.accept(), logger, analytics).start()
    }
  }
}

class RequestError(message: String) extends Exception(message)

class ProxyThread(client: Socket,
                  implicit val logger: Logger,
                  implicit val analytics: Analytics)
  extends Thread("Proxy Thread") {
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
      try {
        RequestLine(line)
        knowPath = false
      } catch {
        case _: HeaderParseError =>
      }

      //logger.log(Level.INFO, s"SOCKET FROM ${client.getRemoteSocketAddress.toString}")

      if (!knowPath) {
        val request = RequestLine(line)
        var path: Path = null
        try {
          path = Path(request.path.slice(1, request.path.length))
          logger.log(Level.INFO, s"The path is ${path.host} : ${path.port}")
          host = path.host
        } catch {
          case _: HeaderParseError =>
            client.close()
            logger.log(Level.SEVERE, "Could not parse path")
            throw new RequestError("Could not parse path")
        }
        try {
          server = SSLSocketFactory.getDefault.createSocket(path.host, 443)
          serverOut = server.getOutputStream
          serverIn = server.getInputStream
        } catch {
          case _: UnknownHostException =>
            client.close()
            return
        }

        request.path = path.absolutePath + path.query
        line = request.toString

        new TransmitterThread(clientOut, serverIn, client, path, analytics, logger).start()
        knowPath = true
      }

      if (line == "") inBody = true

      if (!inBody) {
        try {
          val header = Header(line)
          if (header.key == "Host") header.value = host
          line = header.toString
        } catch {
          case _: HeaderParseError => logger.log(Level.SEVERE, "Failed to parse header")
        }
      }

      serverOut.write((line + "\r\n").toCharArray.map(_.toByte), 0, line.length + 2)
      serverOut.flush()
      logger.log(Level.INFO, s"Wrote $line")
      line = clientIn.readLine()
    }
    logger.log(Level.FINE, "client done")
    client.close()
  }
}

class TransmitterThread(out: OutputStream,
                        in: InputStream,
                        client: Socket,
                        path: Path,
                        analytics: Analytics,
                        implicit val logger: Logger) extends Thread("Transmitter Thread") {
  override def run(): Unit = {
    try {
      val response = new RawHttp().parseResponse(in)
      val h = RawHttpHeaders.newBuilder()
      h.overwrite("X-ReflectJS-Proxied", "1")
      h.overwrite("X-Frame-Options", "none")
      h.overwrite("Access-Control-Allow-Origin", "*")
      h.overwrite("Content-Security-Policy", "default-src 'self' *.reflectjs.com")
      response.withHeaders(h.build()).writeTo(out)
      analytics.addRequest(client.getInetAddress.toString, path)
      logger.log(Level.FINE, "Finished writing request")
    } catch {
      case _: IllegalStateException => client.close()
    }
  }
}
