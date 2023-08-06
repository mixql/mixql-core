package org.mixql.core.test.function

import org.mixql.core.context.gtype._
import org.mixql.core.test.MainVisitorBaseTest

class AsciFunctionsTest extends MainVisitorBaseTest {

  test("Test `ascii('') == 0`") {
    val code =
      """
        |let foo = ascii('');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[gInt]
    assert(foo.getValue == 0)
  }

  test("Test `ascii('234') == 50`") {
    val code =
      """
        |let foo = ascii('234');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[gInt]
    assert(foo.getValue == 50)
  }
}
