package org.mixql.core.test.visitor

import org.mixql.core.test.MainVisitorBaseTest
import org.mixql.core.context.mtype._
import org.mixql.core.engine.Engine
import org.mixql.core.context.{Context, EngineContext}
import org.mixql.core
import org.mixql.core.test.engines.StubEngine

import scala.collection.mutable.{Map => MutMap}
import org.mixql.core.context.mtype.MAsync
import org.mixql.core.function.MLambda

class LambdaTest extends MainVisitorBaseTest {

  test("Test create lambda") {
    val code =
      """
        |let res = (x) -> begin
        |  return 1;
        |  return 2;
        |  1 + 2;
        |end;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MLambda])
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
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1)
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
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1)
    val end = context.getVar("end_var")
    assert(end.isInstanceOf[MInt])
    assert(end.asInstanceOf[MInt].getValue == 12)
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
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1)
    val end = context.getVar("end_var")
    assert(end.isInstanceOf[MInt])
    assert(end.asInstanceOf[MInt].getValue == 12)
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
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1)
    val end = context.getVar("end_var")
    assert(end.isInstanceOf[MInt])
    assert(end.asInstanceOf[MInt].getValue == 12)
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
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 0)
    val end = context.getVar("end_var")
    assert(end.isInstanceOf[MInt])
    assert(end.asInstanceOf[MInt].getValue == 12)
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
      override def executeImpl(stmt: String, ctx: EngineContext): MType = {
        throw new NullPointerException("hello")
      }
    }
    val context = runMainVisitor(code, Context(MutMap("stub" -> new Other), "stub"))
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1)
    val end = context.getVar("end_var")
    assert(end.isInstanceOf[MInt])
    assert(end.asInstanceOf[MInt].getValue == 12)
  }

  test("Test lambda as param of lambda") {
    val code =
      """
        |let lambda = (x) -> begin
        |  return x();
        |end;
        |let x = () -> begin
        |  return 1;
        |end;
        |let res = lambda($x);
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1)
  }

  test("Test return lambda from lambda") {
    val code =
      """
        |let lambda = () -> begin
        |  let x = () -> begin
        |    return 1;
        |  end;
        |  return $x;
        |end;
        |let res = lambda();
        |let res1 = res();
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MLambda])
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == 1)
  }

  test("Test throw exception from lambda") {
    val code =
      """
        |let lambda = () -> begin
        |  RAISE "async", "await";
        |  return 1;
        |end;
        |try
        |  let async_call = lambda();
        |catch exc then
        |  let exc_t = $exc["type"];
        |  let exc_m = $exc["message"];
        |end
                """.stripMargin
    val context = runMainVisitor(code)
    val exc_t = context.getVar("exc_t")
    assert(exc_t.isInstanceOf[MString])
    assert(exc_t.asInstanceOf[MString].getValue == "async")
    val exc_m = context.getVar("exc_m")
    assert(exc_m.isInstanceOf[MString])
    assert(exc_m.asInstanceOf[MString].getValue == "await")
  }

  test("Test throw exception from scala func") {
    val code =
      """
        |try
        |  let exc = await lambda();
        |catch exc then
        |  let exc_t = $exc["type"];
        |  let exc_m = $exc["message"];
        |end
                """.stripMargin
    val lambda: Any =
      new (() => Int) {
        def apply(): Int = throw new Exception("await")
      }
    val context = runMainVisitor(
      code,
      Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub", MutMap[String, Any]("lambda" -> lambda))
    )
    val exc_t = context.getVar("exc_t")
    assert(exc_t.isInstanceOf[MString])
    assert(exc_t.asInstanceOf[MString].getValue == "Exception")
    val exc_m = context.getVar("exc_m")
    assert(exc_m.isInstanceOf[MString])
    assert(exc_m.asInstanceOf[MString].getValue == "await")
  }

  test("Test call async lambda") {
    val code =
      """
        |let lambda = () -> begin
        |  return 1;
        |end;
        |let res = await async lambda();
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1)
  }

  test("Test await from var") {
    val code =
      """
        |let lambda = () -> begin
        |  return 1;
        |end;
        |let async_call = async lambda();
        |let res = await $async_call;
                """.stripMargin
    val context = runMainVisitor(code)
    val async_call = context.getVar("async_call")
    assert(async_call.isInstanceOf[MAsync])
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1)
  }

  test("Test throw async exception from lambda") {
    val code =
      """
        |let lambda = () -> begin
        |  RAISE "async", "await";
        |  return 1;
        |end;
        |let async_call = async lambda();
        |let exc = await $async_call;
        |let exc_t = $exc["type"];
        |let exc_m = $exc["message"];
                """.stripMargin
    val context = runMainVisitor(code)
    val exc_t = context.getVar("exc_t")
    assert(exc_t.isInstanceOf[MString])
    assert(exc_t.asInstanceOf[MString].getValue == "async")
    val exc_m = context.getVar("exc_m")
    assert(exc_m.isInstanceOf[MString])
    assert(exc_m.asInstanceOf[MString].getValue == "await")
  }

  test("Test throw async exception from scala func") {
    val code =
      """
        |let async_call = async lambda();
        |let exc = await $async_call;
        |let exc_t = $exc["type"];
        |let exc_m = $exc["message"];
                """.stripMargin
    val lambda: Any =
      new (() => Int) {
        def apply(): Int = throw new Exception("await")
      }
    val context = runMainVisitor(
      code,
      Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub", MutMap[String, Any]("lambda" -> lambda))
    )
    val exc_t = context.getVar("exc_t")
    assert(exc_t.isInstanceOf[MString])
    assert(exc_t.asInstanceOf[MString].getValue == "Exception")
    val exc_m = context.getVar("exc_m")
    assert(exc_m.isInstanceOf[MString])
    assert(exc_m.asInstanceOf[MString].getValue == "await")
  }

  test("Test aync as param of lambda") {
    val code =
      """
        |let lambda = (x) -> begin
        |  return await $x;
        |end;
        |let x = () -> begin
        |  return 1;
        |end;
        |let asnc = async x();
        |let res = lambda($asnc);
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1)
  }

  test("Test return async from lambda") {
    val code =
      """
        |let lambda = () -> begin
        |  let x = () -> begin
        |    return 1;
        |  end;
        |  return async x();
        |end;
        |let res = lambda();
        |let res1 = await $res;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MAsync])
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == 1)
  }
}
