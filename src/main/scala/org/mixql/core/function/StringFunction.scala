package org.mixql.core.function

import java.nio.charset.StandardCharsets
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale

object StringFunction {
  val ascii =
    new (String => Int) {
      override def apply(str: String): Int =
        if (str.isEmpty)
          0
        else
          str(0).toInt
    }

  val base64 =
    new (String => String) {
      def apply(str: String): String =
        java
          .util
          .Base64
          .getEncoder
          .encodeToString(str.getBytes(StandardCharsets.UTF_8))
    }

  val concat =
    new Object {
      def apply(exprs: String*): String =
        exprs.filterNot(_ == null).mkString("")
    }

  // todo: add support for array https://kontext.tech/article/1079/spark-sql-concatenate-withwithout-separator
  val concat_ws =
    new Object {
      def apply(sep: String, exprs: String*): String =
        exprs.filterNot(_ == null).mkString(sep)
    }

  val length =
    new (String => Int) {
      def apply(str: String): Int = str.length
    }

  val substr =
    new ((String, Int, Int) => String) {
      def apply(str: String, pos: Int, len: Int = Int.MaxValue): String = {
        if (len < 1)
          return ""
        val pre =
          if (pos > 0)
            pos - 1
          else
            str.length + pos
        val p =
          if (pre < 0)
            0
          else
            pre
        if (len == Int.MaxValue)
          str.substring(p)
        else
          str.substring(p, pre + len)
      }
    }

  val formatNumber =
    new ((Double, Int) => String) {
      val defaultFormat = "#,###,###,###,###,###,##0"
      val numberFormat =
        new DecimalFormat("", new DecimalFormatSymbols(Locale.US))
      val pattern: StringBuffer = new StringBuffer()

      // todo: what if scale < 0 ?
      def apply(number: Double, scale: Int): String = {
        pattern.delete(0, pattern.length)
        pattern.append(defaultFormat)
        if (scale < 0)
          return null

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

  val startsWith =
    new ((String, String) => Boolean) {
      def apply(str: String, prefix: String): Boolean = {
        str.startsWith(prefix)
      }
    }

  val endsWith =
    new ((String, String) => Boolean) {
      def apply(str: String, suffix: String): Boolean = {
        str.endsWith(suffix)
      }
    }

  val isEmpty =
    new ((String) => Boolean) {
      def apply(str: String): Boolean = {
        str.isEmpty
      }
    }

  val nonEmpty =
    new ((String) => Boolean) {
      def apply(str: String): Boolean = {
        str.nonEmpty
      }
    }

  val findFirstIn =
    new ((String, String) => Any) {
      def apply(str: String, findPattern: String): Any = {
        findPattern.r.findFirstIn(str) match {
          case None =>
            null
          case Some(v) =>
            v
        }
      }
    }

  val findAllIn =
    new ((String, String) => Array[String]) {
      def apply(str: String, findPattern: String): Array[String] = {
        findPattern.r.findAllIn(str).toList.toArray
      }
    }

  val replaceAllIn =
    new ((String, String, String) => String) {
      def apply(str: String, pattern: String, replacement: String): String = {
        pattern.r.replaceAllIn(str, replacement)
      }
    }

  val replaceFirstIn =
    new ((String, String, String) => String) {
      def apply(str: String, pattern: String, replacement: String): String = {
        pattern.r.replaceFirstIn(str, replacement)
      }
    }

  val split =
    new ((String, String) => Array[String]) {
      def apply(str: String, pattern: String): Array[String] = {
        str.split(pattern)
      }
    }

  val toLowerCase =
    new ((String) => String) {
      def apply(str: String): String = {
        str.toLowerCase
      }
    }

  val toUpperCase =
    new ((String) => String) {
      def apply(str: String): String = {
        str.toUpperCase
      }
    }

  val trim =
    new ((String) => String) {
      def apply(str: String): String = {
        str.trim
      }
    }
}
