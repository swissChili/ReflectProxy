package com.reflectjs

import java.util.logging._
import java.text.SimpleDateFormat
import java.util.Date


class LogFormatter extends Formatter {
  private val reset = 0
  private val black = 30
  private val red = 31
  private val green = 32
  private val yellow = 33
  private val blue = 34

  private def color(num: Int): String = s"\u001B[${num}m"

  private def date(ms: Long): String = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    val resultdate = new Date(ms)
    dateFormat.format(resultdate)
  }

  override def format(r: LogRecord): String = {
    s"${color(green)}${date(r.getMillis)}${color(reset)} ${r.getSourceClassName}/${r.getSourceMethodName}\n" +
    (r.getLevel match {
      case Level.SEVERE => color(red)
      case Level.WARNING => color(yellow)
      case Level.FINE => color(green)
      case Level.INFO => color(blue)
      case _ => ""
    }) + String.format("%10s", r.getLevel.getName) + s"${color(reset)} ${r.getMessage}\n"
  }
}
