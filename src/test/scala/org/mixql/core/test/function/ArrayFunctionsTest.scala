package org.mixql.core.test.function

import org.mixql.core.context.gtype._
import org.mixql.core.test.MainVisitorBaseTest

class ArrayFunctionsTest extends MainVisitorBaseTest {
  test("Test size") {
    val code =
      """
        |let res = size([1,2,3]);
        |""".stripMargin

    val context = runMainVisitor(code)

    val res = context.getVar("res")
    assert(res.isInstanceOf[gInt])
    assert(res.asInstanceOf[gInt].getValue == 3)
  }

  test("Test sort") {
    val code =
      """
        |let res = sort([3,2,1], (x, y) -> begin $x < $y; end);
        |""".stripMargin

    val context = runMainVisitor(code)

    val res = context.getVar("res")
    assert(res.isInstanceOf[array])
    assert(res.asInstanceOf[array].getArr.size == 3)
    assert(res.asInstanceOf[array](new gInt(0)).asInstanceOf[gInt].getValue == 1)
    assert(res.asInstanceOf[array](new gInt(1)).asInstanceOf[gInt].getValue == 2)
    assert(res.asInstanceOf[array](new gInt(2)).asInstanceOf[gInt].getValue == 3)
  }
}
