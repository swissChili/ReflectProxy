package com.reflectjs

import java.sql._
import java.util.logging.{Logger, Level}
import java.util.Date

class Analytics(user: String, passwd: String) {
  val url = "jdbc:mysql://localhost:3306/reflectjs?useSSL=false"

  val connection: Connection = DriverManager.getConnection(url, user, passwd)

  def addRequest(clientIp: String, path: Path)(implicit logger: Logger): Unit = {
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
