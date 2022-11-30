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

  test("Test sort") {
    val code =
      """
        |let res = sort([3,2,1], (x, y) -> begin $x < $y; end);
        |""".stripMargin

    val context = runMainVisitor(code)

    val res = context.getVar("res")
    assert(res.isInstanceOf[array])
    assert(res.asInstanceOf[array].arr.size == 3)
    assert(res.asInstanceOf[array](int(0)).asInstanceOf[int].value == 1)
    assert(res.asInstanceOf[array](int(1)).asInstanceOf[int].value == 2)
    assert(res.asInstanceOf[array](int(2)).asInstanceOf[int].value == 3)
  }
}
