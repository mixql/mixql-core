package org.mixql.core.test.function

import org.mixql.core.context.mtype._
import org.mixql.core.test.MainVisitorBaseTest

class ArrayFunctionsTest extends MainVisitorBaseTest {

  test("Test size") {
    val code =
      """
        |let res = size([1,2,3]);
        |""".stripMargin

    val context = runMainVisitor(code)

    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 3)
  }

  test("Test sort") {
    val code =
      """
        |let res = sort([3,2,1], (x, y) -> begin return $x < $y; end);
        |""".stripMargin

    val context = runMainVisitor(code)

    val res = context.getVar("res")
    assert(res.isInstanceOf[MArray])
    assert(res.asInstanceOf[MArray].getArr.size == 3)
    assert(res.asInstanceOf[MArray](new MInt(0)).asInstanceOf[MInt].getValue == 1)
    assert(res.asInstanceOf[MArray](new MInt(1)).asInstanceOf[MInt].getValue == 2)
    assert(res.asInstanceOf[MArray](new MInt(2)).asInstanceOf[MInt].getValue == 3)
  }
}
