package org.grenki.gsql.test.visitor

import org.grenki.gsql.context.gtype._
import org.grenki.gsql.test.MainVisitorBaseTest

class ExpressionTest extends MainVisitorBaseTest {
  test("Test arithmetic expression") {
    val code =
      """
        |set a = 0.5;
        |set b = 1.5;
        |set res = (($a + $b) * $a) / 2;
                """.stripMargin
    val context = runMainVisitor(code)
    assert(context.vars.contains("res"))
    val res = context.vars("res")
    assert(res.isInstanceOf[double])
    assert(context.vars("res").asInstanceOf[double].value == 0.5)
  }

  test("Test bool expression") {
    val code =
      """
        |set a = 12;
        |set res = $a > 11 and $a < 12;
        |set res1 = $a > 11 or $a < 12;
                """.stripMargin
    val context = runMainVisitor(code)
    assert(context.vars.contains("res"))
    assert(context.vars.contains("res1"))
    val res = context.vars("res")
    assert(res.isInstanceOf[bool])
    assert(res.asInstanceOf[bool].value != true)

    val res1 = context.vars("res1")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].value == true)
  }

  test("Test string expression") {
    val code =
      """
        |set res = 'one' || 2 || true;
                """.stripMargin
    val context = runMainVisitor(code)
    assert(context.vars.contains("res"))
    val res = context.vars("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].value == "one2true")
  }

  test("Test case then expression") {
    val code =
      """
        |set res = case when 2 > 1 then true else 'false' end;
                """.stripMargin
    val context = runMainVisitor(code)
    assert(context.vars.contains("res"))
    val res = context.vars("res")
    assert(res.isInstanceOf[bool])
    assert(res.asInstanceOf[bool].value == true)
  }

  test("Test case else expression") {
    val code =
      """
        |set res = case when 2 < 1 then true else 'false' end;
                """.stripMargin
    val context = runMainVisitor(code)
    assert(context.vars.contains("res"))
    val res = context.vars("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].value == "false")
  }
}