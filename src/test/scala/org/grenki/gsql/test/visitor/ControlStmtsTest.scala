package org.grenki.gsql.test.visitor

import org.grenki.gsql.test.MainVisitorBaseTest
import org.grenki.gsql.context.gtype.string
import org.grenki.gsql.engine.Engine
import org.grenki.gsql.context.gtype.Type
import org.grenki.gsql.context.Context

import scala.collection.mutable.{Map => MutMap}
import org.grenki.gsql.test.stub.StubEngine

class ControlStmtsTest extends MainVisitorBaseTest {
  test("Test if: then") {
    val code =
      """
        |if true != false then
        |  let res = "if";
        |elif false then
        |  let res = "elif";
        |else
        |  let res = "else";
        |end if
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].value == "if")
  }

  test("Test if: elif") {
    val code =
      """
        |if false then
        |  let res = "if";
        |elif true then
        |  let res = "elif";
        |else
        |  let res = "else";
        |end if
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].value == "elif")
  }

  test("Test if: else") {
    val code =
      """
        |if not true then
        |  let res = "if";
        |elif false == true then
        |  let res = "elif";
        |else
        |  let res = "else";
        |end if
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].value == "else")
  }

  test("Test while") {
    val code =
      """
        |let x = 0;
        |let res = "";
        |while $x < 5 do
        |  let res = $res || $x;
        |  let x = $x + 1;
        |end
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].value == "01234")
  }

  test("Test for range") {
    val code =
      """
        |let res = "";
        |for i in 1..20 step 2 loop
        |  let res = $res || $i;
        |end loop
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].value == "13579111315171920")
  }

  test("Test for range reverse") {
    val code =
      """
        |let res = "";
        |for i in REVERSE 1..20 step 2 loop
        |  let res = $res || $i;
        |end loop
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].value == "20181614121086421")
  }

  test("Test change engine") {
    val code =
      """
        |let engine "stub" || 1();
                """.stripMargin
    class Other extends Engine {
      override def name: String = "other"

      override def execute(stmt: String): Type = ???

      override def setParam(name: String, value: Type): Unit = ???

      override def getParam(name: String): Type = ???

    }
    val context = runMainVisitor(
      code,
      new Context(
        MutMap("stub" -> new StubEngine, "stub1" -> new Other),
        "stub"
      )
    )

    assert(context.currentEngine.isInstanceOf[Other])
    assert(context.currentEngine.name == "other")
    assert(context.currentEngineAllias == "stub1")
  }

  test("Test change engine params") {
    val code =
      """
        |let engine stub(spark.execution.memory="16G");
                """.stripMargin
    class Other extends Engine {
      override def name: String = "other"

      override def execute(stmt: String): Type = ???

      override def setParam(name: String, value: Type): Unit = ???

      override def getParam(name: String): Type = ???

    }
    val context = runMainVisitor(
      code,
      new Context(
        MutMap("stub" -> new StubEngine, "stub1" -> new Other),
        "stub"
      )
    )

    assert(context.currentEngine.isInstanceOf[StubEngine])
    assert(context.currentEngine.name == "stub")
    assert(context.currentEngineAllias == "stub")
    assert(
      context.currentEngine
        .getParam("spark.execution.memory")
        .toString() == "16G"
    )
  }
}
