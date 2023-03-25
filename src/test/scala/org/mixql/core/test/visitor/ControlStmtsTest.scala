package org.mixql.core.test.visitor

import org.mixql.core.test.MainVisitorBaseTest
import org.mixql.core.context.gtype._
import org.mixql.core.engine.Engine
import org.mixql.core.context.Context
import org.mixql.core
import org.mixql.core.test.stub.StubEngine

import scala.collection.mutable.{Map => MutMap}

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
        |end while
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
    val i = context.getVar("i")
    assert(isNull(i))
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
    val i = context.getVar("i")
    assert(isNull(i))
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].value == "20181614121086421")
  }

  test("Test for in cursor: array") {
    val code =
      """
        |let res = "";
        |for i in [1, 3, 5] loop
        |  let res = $res || $i;
        |end loop
                """.stripMargin
    val context = runMainVisitor(code)
    val i = context.getVar("i")
    assert(isNull(i))
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].value == "135")
  }

  test("Test change engine") {
    val code =
      """
        |let engine "stub" || 1;
                """.stripMargin
    class Other extends Engine {
      override def name: String = "other"

      override def execute(stmt: String): Type = ???

      override def executeFunc(name: String, params: Type*): Type = ???

      override def setParam(name: String, value: Type): Unit = {}

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
    class Other extends StubEngine {
      override def name: String = "other"
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

  test("Test run on other engine with params") {
    val code =
      """
        |select gg from wp on engine stub1(spark.execution.memory="16G");
                """.stripMargin
    class Other extends StubEngine {
      var old: Map[String, Type] = Map()
      param.put("spark.execution.memory", string("8G"))
      override def name: String = "other"
      override def execute(stmt: String): Type = {
        queue += stmt
        old = param.toMap
        Null
      }
    }
    val stub1 = new Other
    val context = runMainVisitor(
      code,
      new Context(MutMap("stub" -> new StubEngine, "stub1" -> stub1), "stub")
    )

    assert(context.currentEngine.isInstanceOf[StubEngine])
    assert(context.currentEngine.name == "stub")
    assert(context.currentEngineAllias == "stub")
    assert(
      stub1
        .getParam("spark.execution.memory")
        .toString() == "8G"
    )
    assert(
      stub1
        .old("spark.execution.memory")
        .toString() == "16G"
    )
  }

  test("Test try/catch") {
    val code =
      """
        |TRY
        |  select gg from wp;
        |CATCH ex THEN
        |  let res = $ex;
        |  let res_msg = $ex.message;
        |END
                """.stripMargin

    class Other extends StubEngine {
      override def execute(stmt: String): Type = {
        throw new NullPointerException("hello")
      }
    }
    val context =
      runMainVisitor(code, new Context(MutMap("stub" -> new Other), "stub"))
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].value == "NullPointerException")
    val res_msg = context.getVar("res_msg")
    assert(res_msg.isInstanceOf[string])
    assert(res_msg.asInstanceOf[string].value == "hello")

    assert(isNull(context.getVar("ex")))
    assert(isNull(context.getVar("ex.message")))
  }

  test("Test return") {
    val code =
      """
        |let x = 1;
        |let y = 2;
        |return $x;
        |return $y;
        |$x + $y;
                """.stripMargin
    val res = core.run(
      code,
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    )
    assert(res.isInstanceOf[int])
    assert(res.asInstanceOf[int].value == 1)
  }

  test("Test return from lambda") {
    val code =
      """
        |let test_ret = (x) -> begin 
        |  return 1;
        |  return 2;
        |  1 + 2;
        |end;
        |let res = test_ret(1);
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[int])
    assert(res.asInstanceOf[int].value == 1)
  }

  test("Test multiple assigment") {
    val code =
      """
        |let res1, res2 = 1, 2, 3;
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[int])
    assert(res1.asInstanceOf[int].value == 1)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[int])
    assert(res2.asInstanceOf[int].value == 2)
  }

  test("Test multiple assigment: unpack array") {
    val code =
      """
        |let res1, res2 = [1, 2, 3];
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[int])
    assert(res1.asInstanceOf[int].value == 1)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[int])
    assert(res2.asInstanceOf[int].value == 2)
  }

  test("Test multiple assigment: foreach") {
    val code =
      """
        |let res = 0;
        |let c = [[1, 2, 4], [3, 4, 8], [5, 6, 9]];
        |for a, b in $c loop
        | let res = $res + $a + $b;
        |end loop
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[int])
    assert(res.asInstanceOf[int].value == 21)
  }
}
