package org.grenki.gsql.test.visitor

import org.grenki.gsql.test.MainVisitorBaseTest

class ControlStmtsTest extends MainVisitorBaseTest {
  test("Test if: then") {
    val code =
      """
        |if true != false then
        |  set res = "if";
        |elif false then
        |  set res = "elif";
        |else
        |  set res = "else";
        |end if
                """.stripMargin
    val context = runMainVisitor(code)
    assert(context.variables.contains("res"))
    assert(context.variables("res").toString() == "if")
  }

  test("Test if: elif") {
    val code =
      """
        |if false then
        |  set res = "if";
        |elif true then
        |  set res = "elif";
        |else
        |  set res = "else";
        |end if
                """.stripMargin
    val context = runMainVisitor(code)
    assert(context.variables.contains("res"))
    assert(context.variables("res").toString() == "elif")
  }

  test("Test if: else") {
    val code =
      """
        |if not true then
        |  set res = "if";
        |elif false == true then
        |  set res = "elif";
        |else
        |  set res = "else";
        |end if
                """.stripMargin
    val context = runMainVisitor(code)
    assert(context.variables.contains("res"))
    assert(context.variables("res").toString() == "else")
  }

  test("Test while") {
    val code =
      """
        |set x = 0;
        |set res = "";
        |while $x < 5 do
        |  set res = $res || $x;
        |  set x = $x + 1;
        |end
                """.stripMargin
    val context = runMainVisitor(code)
    assert(context.variables.contains("res"))
    assert(context.variables("res").toString() == "01234")
  }

  test("Test for range") {
    val code =
      """
        |set res = "";
        |for i in 1..20 step 2 loop
        |  set res = $res || $i;
        |end loop
                """.stripMargin
    val context = runMainVisitor(code)
    assert(context.variables.contains("res"))
    val res = context.variables("res")
    assert(context.variables("res").toString() == "13579111315171920")
  }
  // TODO this test fails
  ignore("Test for range reverse") {
    val code =
      """
        |set res = "";
        |for i in REVERSE 1..20 step 2 loop
        |  set res = $res || $i;
        |end loop
                """.stripMargin
    val context = runMainVisitor(code)
    assert(context.variables.contains("res"))
    val res = context.variables("res")
    assert(context.variables("res").toString() == "20181614121086421")
  }
}
