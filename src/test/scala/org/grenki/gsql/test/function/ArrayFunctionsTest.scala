package org.grenki.gsql.test.function

import org.grenki.gsql.context.gtype._
import org.grenki.gsql.test.MainVisitorBaseTest

class ArrayFunctionsTest extends MainVisitorBaseTest {
  test("Test size") {
    val code =
      """
        |let res = size([1,2,3]);
        |""".stripMargin

    val context = runMainVisitor(code)

    val res = context.getVar("res")
    assert(res.isInstanceOf[int])
    assert(res.asInstanceOf[int].value == 3)
  }
}
