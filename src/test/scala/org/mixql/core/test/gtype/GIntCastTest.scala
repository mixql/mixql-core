package org.mixql.core.test.gtype

import org.mixql.core.context.gtype.{gDouble, gInt, string}
import org.mixql.core.test.MainVisitorBaseTest

class GIntCastTest extends MainVisitorBaseTest {

  test("Test cast double to int") {
    Int.MaxValue
    val code =
      """
        |let res = cast(12.5 as int);
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[gInt])
    assert(res1.asInstanceOf[gInt].getValue == 12)
  }

  test("Test cast int to double") {
    Int.MaxValue
    val code =
      """
        |let res = cast(12 as double);
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[gDouble])
    assert(res1.asInstanceOf[gDouble].getValue == 12.0)
  }

  test("Test cast string to int") {

    Int.MaxValue
    val code =
      """
        |let res = cast("12" as int);
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[gInt])
    assert(res1.asInstanceOf[gInt].getValue == 12)
  }

  test("Test int to string") {

    Int.MaxValue
    val code =
      """
        |let res = 100 + "";
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[string])
    assert(res1.asInstanceOf[string].getValue == "100")
  }

  test("Test double int to string") {
    Int.MaxValue
    val code =
      """
        |let res = cast(12.5 as string);
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[string])
    assert(res1.asInstanceOf[string].getValue == "12.5")
  }

  test("Test cast int to string") {
    Int.MaxValue
    val code =
      """
        |let res = cast(123 as string);
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[string])
    assert(res1.asInstanceOf[string].getValue == "123")
  }
}
