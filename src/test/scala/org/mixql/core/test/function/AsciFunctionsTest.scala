package org.mixql.core.test.function

import org.mixql.core.context.gtype.{int, string}
import org.mixql.core.test.MainVisitorBaseTest

class AsciFunctionsTest extends MainVisitorBaseTest {
  test("Test `ascii('') == 0`") {
    val code =
      """
        |let foo = ascii('');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[int]
    assert(foo.value == 0)
  }

  test("Test `ascii('234') == 50`") {
    val code =
      """
        |let foo = ascii('234');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[int]
    assert(foo.value == 50)
  }
}
