package org.mixql.core.test.visitor

import com.typesafe.config.ConfigFactory
import org.mixql.core.test.MainVisitorBaseTest
import org.mixql.core.context.gtype._
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
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "if")
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
    assert(res.asInstanceOf[string].getValue == "elif")
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
    assert(res.asInstanceOf[string].getValue == "else")
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
    assert(res.asInstanceOf[string].getValue == "01234")
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
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "")
    val x = context.getVar("x")
    assert(x.isInstanceOf[gInt])
    assert(x.asInstanceOf[gInt].getValue == 5)
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
    assert(res.isInstanceOf[gInt])
    assert(res.asInstanceOf[gInt].getValue == 1)
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
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "13579111315171920")
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
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "20181614121086421")
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
    assert(i.isInstanceOf[gInt])
    assert(i.asInstanceOf[gInt].getValue == 11)
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "")
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
    assert(i.isInstanceOf[gInt])
    assert(i.asInstanceOf[gInt].getValue == 1)
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "")
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
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "135")
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
    assert(count.isInstanceOf[gInt])
    assert(count.asInstanceOf[gInt].getValue == 3)
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "")
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
    assert(count.isInstanceOf[gInt])
    assert(count.asInstanceOf[gInt].getValue == 1)
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "")
  }

  test("Test change engine") {
    val code =
      """
        |let engine "stub" || 1;
                """.stripMargin
    class Other extends Engine {
      override def name: String = "other"

      override def executeImpl(stmt: String, ctx: EngineContext): Type = ???

      override def executeFuncImpl(name: String, ctx: EngineContext, kwargs: Map[String, Object], params: Type*): Type =
        ???

    }
    val context = runMainVisitor(code, Context(MutMap("stub" -> new StubEngine, "stub1" -> new Other), "stub"))

    assert(context.currentEngine.isInstanceOf[Other])
    assert(context.currentEngine.name == "other")
    assert(context.currentEngineAllias == "stub1")
  }

  test("Test change engine params") {
    //    assume({
    //      val config = ConfigFactory.load()
    //      Try({
    //        val param = config.getString("mixql.engine.variables.update")
    //        if (param.trim != "all") false
    //        else true
    //      }).getOrElse(true)
    //    })
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

      override def executeImpl(stmt: String, ctx: EngineContext): Type = {
        queue += stmt + " spark.execution.memory=" + ctx.getVar("spark.execution.memory").toString
        new Null()
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
      override def executeImpl(stmt: String, ctx: EngineContext): Type = {
        queue += stmt
        sem = ctx.context.getParams("stub1")("spark.execution.memory").toString
        new Null()
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
      override def executeImpl(stmt: String, ctx: EngineContext): Type = {
        throw new NullPointerException("hello")
      }
    }
    val context = runMainVisitor(code, Context(MutMap("stub" -> new Other), "stub"))
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "NullPointerException")
    val res_msg = context.getVar("res_msg")
    assert(res_msg.isInstanceOf[string])
    assert(res_msg.asInstanceOf[string].getValue == "hello")

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
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "gg")
    val res_msg = context.getVar("res_msg")
    assert(res_msg.isInstanceOf[string])
    assert(res_msg.asInstanceOf[string].getValue == "wp")

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
    assert(res.isInstanceOf[gInt])
    assert(res.asInstanceOf[gInt].getValue == 1)
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
    assert(res.isInstanceOf[gInt])
    assert(res.asInstanceOf[gInt].getValue == 1)
  }

  test("Test return from lambda with while cycle") {
    val code =
      """
        |let test_ret = (x) -> begin
        |   let x = 1;
        |   while $x < 5 do
        |     return $x;
        |     let x = $x + 1;
        |   end while
        |   return 2;
        |end;
        |let res = test_ret(1);
        |let end_var = 12;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[gInt])
    assert(res.asInstanceOf[gInt].getValue == 1)
    val end = context.getVar("end_var")
    assert(end.isInstanceOf[gInt])
    assert(end.asInstanceOf[gInt].getValue == 12)
  }

  test("Test return from lambda with for range") {
    val code =
      """
        |let test_ret = (x) -> begin
        |   let x = 0;
        |   for i in 1..20 step 2 loop
        |     return $i;
        |   end loop
        |   return 2;
        |end;
        |let res = test_ret(1);
        |let end_var = 12;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[gInt])
    assert(res.asInstanceOf[gInt].getValue == 1)
    val end = context.getVar("end_var")
    assert(end.isInstanceOf[gInt])
    assert(end.asInstanceOf[gInt].getValue == 12)
  }

  test("Test return from lambda with for in cursor") {
    val code =
      """
        |let test_ret = (x) -> begin
        |   let x = 0;
        |   for i in [1, 3, 5] loop
        |     return $i;
        |   end loop
        |   return 2;
        |end;
        |let res = test_ret(1);
        |let end_var = 12;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[gInt])
    assert(res.asInstanceOf[gInt].getValue == 1)
    val end = context.getVar("end_var")
    assert(end.isInstanceOf[gInt])
    assert(end.asInstanceOf[gInt].getValue == 12)
  }

  test("Test return from lambda with try/catch: try") {
    val code =
      """
        |let test_ret = (x) -> begin
        |   TRY
        |     return 0;
        |   CATCH ex THEN
        |     return 1;
        |   END
        |   return 2;
        |end;
        |let res = test_ret(1);
        |let end_var = 12;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[gInt])
    assert(res.asInstanceOf[gInt].getValue == 0)
    val end = context.getVar("end_var")
    assert(end.isInstanceOf[gInt])
    assert(end.asInstanceOf[gInt].getValue == 12)
  }

  test("Test return from lambda with try/catch: catch") {
    val code =
      """
        |let test_ret = (x) -> begin
        |   TRY
        |     select gg from wp;
        |   CATCH ex THEN
        |     return 1;
        |   END
        |   return 2;
        |end;
        |let res = test_ret(1);
        |let end_var = 12;
                """.stripMargin
    class Other extends StubEngine {
      override def executeImpl(stmt: String, ctx: EngineContext): Type = {
        throw new NullPointerException("hello")
      }
    }
    val context = runMainVisitor(code, Context(MutMap("stub" -> new Other), "stub"))
    val res = context.getVar("res")
    assert(res.isInstanceOf[gInt])
    assert(res.asInstanceOf[gInt].getValue == 1)
    val end = context.getVar("end_var")
    assert(end.isInstanceOf[gInt])
    assert(end.asInstanceOf[gInt].getValue == 12)
  }

  test("Test multiple assigment") {
    val code =
      """
        |let res1, res2 = 1, 2, 3;
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[gInt])
    assert(res1.asInstanceOf[gInt].getValue == 1)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[gInt])
    assert(res2.asInstanceOf[gInt].getValue == 2)
  }

  test("Test multiple assigment: unpack array") {
    val code =
      """
        |let res1, res2 = [1, 2, 3];
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[gInt])
    assert(res1.asInstanceOf[gInt].getValue == 1)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[gInt])
    assert(res2.asInstanceOf[gInt].getValue == 2)
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
    assert(res.isInstanceOf[gInt])
    assert(res.asInstanceOf[gInt].getValue == 21)
  }
}
