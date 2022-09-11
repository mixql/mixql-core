package org.grenki.gsql.function

object StringFunction {
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
