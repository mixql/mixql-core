package org.grenki.gsql.function

import java.nio.charset.StandardCharsets
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale

object StringFunction {
  val ascii = new (String => Int) {
    override def apply(str: String): Int =
      if (str.isEmpty) 0
      else str(0).toInt
  }

  val base64 = new (String => String) {
    def apply(str: String): String = java.util.Base64.getEncoder
      .encodeToString(str.getBytes(StandardCharsets.UTF_8))
  }

  val concat = new Object {
    def apply(exprs: String*): String = exprs.filterNot(_ == null).mkString("")
  }

  // todo: add support for array https://kontext.tech/article/1079/spark-sql-concatenate-withwithout-separator
  val concat_ws = new Object {
    def apply(sep: String, exprs: String*): String =
      exprs.filterNot(_ == null).mkString(sep)
  }

  val length = new (String => Int) {
    def apply(str: String): Int = str.length
  }

  val substr = new ((String, Int, Int) => String) {
    def apply(str: String, pos: Int, len: Int = Int.MaxValue): String = {
      if (len < 1) return ""
      val pre = if (pos > 0) pos - 1 else str.length + pos
      val p = if (pre < 0) 0 else pre
      if (len == Int.MaxValue)
        str.substring(p)
      else
        str.substring(p, pre + len)
    }
  }

  val formatNumber = new ((Double, Int) => String) {
    val defaultFormat = "#,###,###,###,###,###,##0"
    val numberFormat =
      new DecimalFormat("", new DecimalFormatSymbols(Locale.US))
    val pattern: StringBuffer = new StringBuffer()

    // todo: what if scale < 0 ?
    def apply(number: Double, scale: Int): String = {
      pattern.delete(0, pattern.length)
      pattern.append(defaultFormat)
      if (scale < 0) return null

      pattern.append(".")
      var i = 0
      while (i < scale) {
        i += 1
        pattern.append("0")
      }
      numberFormat.applyLocalizedPattern(pattern.toString)
      numberFormat.format(number)
    }
  }
}
