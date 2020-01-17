package com.reflectjs

import java.sql._
import java.util.logging.{Logger, Level}
import java.util.Date

abstract class Analytics {
  def addRequest(clientIp: String, path: Path)(implicit logger: Logger)
}

class UselessAnalytics extends Analytics {
  override def addRequest(clientIp: String, path: Path)(implicit logger: Logger): Unit = {
    // do nothing
    logger.log(Level.INFO, "Useless Analytics does not log!")
  }
}

class MySQLAnalytics(user: String, passwd: String) extends Analytics {
  val url = "jdbc:mysql://localhost:3306/reflectjs?useSSL=false"

  val connection: Connection = DriverManager.getConnection(url, user, passwd)

  override def addRequest(clientIp: String, path: Path)(implicit logger: Logger): Unit = {
    val ps = connection.prepareStatement(
      """insert into requests (requested_at, host, path, query, client_ip)
        |VALUES(?, ?, ?, ?, ?);
        |""".stripMargin)

    ps.setTimestamp(1, new Timestamp(new Date().getTime));
    ps.setString(2, path.host)
    ps.setString(3, path.absolutePath)
    ps.setString(4, path.query)
    ps.setString(5, clientIp)

    try {
      ps.executeUpdate()
    } catch {
      case _: SQLException => logger.log(Level.SEVERE, "SQL Exception")
    }
  }
}
