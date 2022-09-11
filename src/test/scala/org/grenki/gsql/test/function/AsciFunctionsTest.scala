package org.grenki.gsql.test.function

import org.grenki.gsql.context.gtype.{int, string}
import org.grenki.gsql.test.MainVisitorBaseTest

class AsciFunctionsTest extends MainVisitorBaseTest {
  test("Test `ascii('') == 0`") {
    val code =
      """
        |set foo = ascii('');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[int]
    assert(foo.value == 0)
  }


  test("Test `ascii('234') == 50`") {
    val code =
      """
        |set foo = ascii('234');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[int]
    assert(foo.value == 50)
  }
}