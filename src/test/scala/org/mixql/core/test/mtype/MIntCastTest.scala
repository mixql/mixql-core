package org.mixql.core.test.mtype

import org.mixql.core.context.mtype._
import org.mixql.core.test.MainVisitorBaseTest

class MIntCastTest extends MainVisitorBaseTest {

  test("Test cast double to int") {
    Int.MaxValue
    val code =
      """
        |let res = cast(12.5 as int);
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == 12)
  }

  test("Test cast int to double") {
    Int.MaxValue
    val code =
      """
        |let res = cast(12 as double);
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MDouble])
    assert(res1.asInstanceOf[MDouble].getValue == 12.0)
  }

  test("Test cast string to int") {

    Int.MaxValue
    val code =
      """
        |let res = cast("12" as int);
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == 12)
  }

  test("Test int to string") {

    Int.MaxValue
    val code =
      """
        |let res = 100 + "";
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MString])
    assert(res1.asInstanceOf[MString].getValue == "100")
  }

  test("Test double int to string") {
    Int.MaxValue
    val code =
      """
        |let res = cast(12.5 as string);
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MString])
    assert(res1.asInstanceOf[MString].getValue == "12.5")
  }

  test("Test cast int to string") {
    Int.MaxValue
    val code =
      """
        |let res = cast(123 as string);
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MString])
    assert(res1.asInstanceOf[MString].getValue == "123")
  }
}
