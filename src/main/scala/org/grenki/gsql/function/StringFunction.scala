package org.grenki.gsql.function

import java.nio.charset.StandardCharsets

object StringFunction {
  val ascii = new (String => Int) {
    override def apply(str: String): Int =
      if (str.isEmpty) 0
      else str(0).toInt
  }

  val base64 = new (String => String) {
    override def apply(str: String): String = java.util.Base64.getEncoder.encodeToString(str.getBytes(StandardCharsets.UTF_8))
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
}
