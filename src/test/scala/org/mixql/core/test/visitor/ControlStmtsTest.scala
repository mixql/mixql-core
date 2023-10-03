package org.mixql.core.test.visitor

import org.mixql.core.test.MainVisitorBaseTest
import org.mixql.core.context.mtype._
import org.mixql.core.engine.Engine
import org.mixql.core.context.{Context, EngineContext}
import org.mixql.core
import org.mixql.core.test.engines.StubEngine

import scala.collection.mutable.{Map => MutMap}
import scala.util.Try

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
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "if")
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
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "elif")
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
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "else")
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
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "01234")
  }

  test("Test while, continue") {
    val code =
      """
        |let x = 0;
        |let res = "";
        |while $x < 5 do
        |  let x = $x + 1;
        |  continue;
        |  let res = $res || $x;
        |end while
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "")
    val x = context.getVar("x")
    assert(x.isInstanceOf[MInt])
    assert(x.asInstanceOf[MInt].getValue == 5)
  }

  test("Test while break") {
    val code =
      """
        |let x = 0;
        |let res = "";
        |while $x < 5 do
        |  let x = $x + 1;
        |  break;
        |  let res = $res || $x;
        |end while
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("x")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1)
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
    assert(isNone(i))
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "13579111315171920")
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
    assert(isNone(i))
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "20181614121086421")
  }

  test("Test for range continue") {
    val code =
      """
        |let res = "";
        |let count = 0;
        |for i in 1..20 step 2 loop
        |  let count = $count + 1;
        |  continue;
        |  let res = $res || $i;
        |end loop
                """.stripMargin
    val context = runMainVisitor(code)
    val i = context.getVar("count")
    assert(i.isInstanceOf[MInt])
    assert(i.asInstanceOf[MInt].getValue == 11)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "")
  }

  test("Test for range break") {
    val code =
      """
        |let res = "";
        |let count = 0;
        |for i in 1..20 step 2 loop
        |  let count = $count + 1;
        |  break;
        |  let res = $res || $i;
        |end loop
                """.stripMargin
    val context = runMainVisitor(code)
    val i = context.getVar("count")
    assert(i.isInstanceOf[MInt])
    assert(i.asInstanceOf[MInt].getValue == 1)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "")
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
    assert(isNone(i))
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "135")
  }

  test("Test for in cursor continue: array") {
    val code =
      """
        |let res = "";
        |let count = 0;
        |for i in [1, 3, 5] loop
        |  let count = $count + 1;
        |  continue;
        |  let res = $res || $i;
        |end loop
                """.stripMargin
    val context = runMainVisitor(code)
    val count = context.getVar("count")
    assert(count.isInstanceOf[MInt])
    assert(count.asInstanceOf[MInt].getValue == 3)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "")
  }

  test("Test for in cursor break: array") {
    val code =
      """
        |let res = "";
        |let count = 0;
        |for i in [1, 3, 5] loop
        |  let count = $count + 1;
        |  break;
        |  let res = $res || $i;
        |end loop
                """.stripMargin
    val context = runMainVisitor(code)
    val count = context.getVar("count")
    assert(count.isInstanceOf[MInt])
    assert(count.asInstanceOf[MInt].getValue == 1)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "")
  }

  test("Test change engine") {
    val code =
      """
        |let engine "stub" || 1;
                """.stripMargin
    class Other extends Engine {
      override def name: String = "other"

      override def executeImpl(stmt: String, ctx: EngineContext): MType = ???

      override def executeFuncImpl(name: String,
                                   ctx: EngineContext,
                                   kwargs: Map[String, Object],
                                   params: MType*): MType = ???

    }
    val context = runMainVisitor(code, Context(MutMap("stub" -> new StubEngine, "stub1" -> new Other), "stub"))

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

    val context = runMainVisitor(code, Context(MutMap("stub" -> new StubEngine, "stub1" -> new Other), "stub"))

    assert(context.currentEngine.isInstanceOf[StubEngine])
    assert(context.currentEngine.name == "stub")
    assert(context.currentEngineAllias == "stub")
    assert(context.getParams()("spark.execution.memory").toString == "16G")
  }

  test("Test engine context vars") {
    val code =
      """
        |let engine stub;
        |
        |let spark.execution.memory="16G";
        |
        |select * from table that does not exist:) using param;
        |
                """.stripMargin
    class Other extends StubEngine {
      override def name: String = "stub"

      override def executeImpl(stmt: String, ctx: EngineContext): MType = {
        queue += stmt + " spark.execution.memory=" + ctx.getVar("spark.execution.memory").toString
        MNull.get()
      }
    }

    val context = runMainVisitor(code, Context(MutMap("stub" -> new Other), "stub"))

    assert(context.currentEngine.isInstanceOf[Other])
    assert(context.currentEngine.name == "stub")
    assert(context.currentEngineAllias == "stub")
    assert(
      context.currentEngine.asInstanceOf[Other].queue.last == "select * from table that does not exist:) using param " +
        "spark.execution.memory=16G"
    )
  }

  test("Test run on other engine with params") {
    class StubEngineWithParam extends StubEngine {
      var sem = ""
      override def executeImpl(stmt: String, ctx: EngineContext): MType = {
        queue += stmt
        sem = ctx.context.getParams("stub1")("spark.execution.memory").toString
        MNull.get()
      }
    }
    val code =
      """
        |let spark.execution.memory = "8G";
        |select gg from wp on engine stub1(spark.execution.memory="16G");
                """.stripMargin
    val stub1 = new StubEngineWithParam
    val context = runMainVisitor(code, Context(MutMap("stub" -> new StubEngine, "stub1" -> stub1), "stub"))

    assert(context.currentEngine.isInstanceOf[StubEngine])
    assert(context.currentEngine.name == "stub")
    assert(context.currentEngineAllias == "stub")
    // Was executed on stub1 engine and not on stub
    assert(context.getEngine("stub1").get.asInstanceOf[StubEngine].queue.last == "select gg from wp")
    assertThrows[Exception](context.getEngine("stub").get.asInstanceOf[StubEngine].queue.last)
    assert(stub1.sem == "16G")
  }

  test("Test run on engine with params") {
    val code =
      """
        |let spark.execution.memory = "8G";
        |dsf s fsd fsfd f; -- trigger engine, engineStarted flag becomes true
        |let engine stub1(spark.execution.memory="16G"); --notify engine that params changed
        |--ignores parameter mixql.engine.variables.update even if it was false
        |--this parameter can be accessed by engine's context
        |select gg from wp; -- executes with new parameter if engine decided to use it
                """.stripMargin

    val context = runMainVisitor(code, Context(MutMap("stub" -> new StubEngine, "stub1" -> new StubEngine), "stub1"))

    assert(context.currentEngine.isInstanceOf[StubEngine])
    assert(context.currentEngine.name == "stub")
    assert(context.currentEngineAllias == "stub1")
    // Was executed on stub1 engine and not on stub
    assert(context.getEngine("stub1").get.asInstanceOf[StubEngine].queue.last == "select gg from wp")
    assertThrows[Exception](context.getEngine("stub").get.asInstanceOf[StubEngine].queue.last)
    assert(context.getParams()("spark.execution.memory").toString == "16G")
  }

  test("Test try/catch") {
    val code =
      """
        |TRY
        |  select gg from wp;
        |CATCH ex THEN
        |  let res = $ex["type"];
        |  let res_msg = $ex["message"];
        |END
                """.stripMargin

    class Other extends StubEngine {
      override def executeImpl(stmt: String, ctx: EngineContext): MType = {
        throw new NullPointerException("hello")
      }
    }
    val context = runMainVisitor(code, Context(MutMap("stub" -> new Other), "stub"))
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "NullPointerException")
    val res_msg = context.getVar("res_msg")
    assert(res_msg.isInstanceOf[MString])
    assert(res_msg.asInstanceOf[MString].getValue == "hello")

    assert(isNone(context.getVar("ex")))
  }

  test("Test try/catch: user exception") {
    val code =
      """
        |TRY
        |  raise "gg", "wp";
        |CATCH ex THEN
        |  let res = $ex["type"];
        |  let res_msg = $ex["message"];
        |END
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "gg")
    val res_msg = context.getVar("res_msg")
    assert(res_msg.isInstanceOf[MString])
    assert(res_msg.asInstanceOf[MString].getValue == "wp")

    assert(isNone(context.getVar("ex")))
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
    val res = core.run(code, Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub"))
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1)
  }

  test("Test multiple assigment") {
    val code =
      """
        |let res1, res2 = 1, 2, 3;
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == 1)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[MInt])
    assert(res2.asInstanceOf[MInt].getValue == 2)
  }

  test("Test multiple assigment: unpack array") {
    val code =
      """
        |let res1, res2 = [1, 2, 3];
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == 1)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[MInt])
    assert(res2.asInstanceOf[MInt].getValue == 2)
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
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 21)
  }
}
