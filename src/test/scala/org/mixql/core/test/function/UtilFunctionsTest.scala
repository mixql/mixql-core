package org.mixql.core.test.function

import org.mixql.core.context.mtype._
import org.mixql.core.test.MainVisitorBaseTest
import org.mixql.core.engine.Engine
import org.mixql.core.context.{Context, EngineContext}
import org.mixql.core.test.engines.StubEngine

import scala.collection.mutable.{Map => MutMap}
import org.mixql.core.exception.MException

class UtilFunctionsTest extends MainVisitorBaseTest {

  test("Test is_error") {
    val code =
      """
        |let lambda = () -> begin
        |  RAISE "async", "await";
        |  return 1;
        |end;
        |let ex = await async lambda();
        |let x = 1;
        |let res1 = is_error($ex);
        |let res2 = is_error($x);
        |""".stripMargin

    val context = runMainVisitor(code)

    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MBool])
    assert(res1.asInstanceOf[MBool].getValue)

    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[MBool])
    assert(!res2.asInstanceOf[MBool].getValue)
  }

  test("Test is_error scala exception") {
    val code =
      """
        |let ex = await async lambda();
        |let res = is_error($ex);
        |""".stripMargin
    val lambda: Object =
      new (() => Int) {
        def apply(): Int = throw new Exception("await")
      }
    val context = runMainVisitor(
      code,
      Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub", MutMap[String, Object]("lambda" -> lambda))
    )

    val res = context.getVar("res")
    assert(res.isInstanceOf[MBool])
    assert(res.asInstanceOf[MBool].getValue)
  }

  test("Test await") {
    val code =
      """
        |let lambda = () -> begin
        |  return 1;
        |end;
        |let ex = async lambda();
        |let res = await($ex);
        |""".stripMargin

    val context = runMainVisitor(code)

    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1)
  }

  test("Test await_all") {
    val code =
      """
        |let async1 = async
        |  raise "gg", "wp";
        |end async;
        |
        |let async2 = async
        |  return 1;
        |end async;
        |
        |let res1, res2 = await_all($async1, $async2);
        |""".stripMargin

    val context = runMainVisitor(code)

    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MException])
    assert(res1.asInstanceOf[MException].getMessage == "wp")

    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[MInt])
    assert(res2.asInstanceOf[MInt].getValue == 1)
  }

  test("Test await_any") {
    val code =
      """
        |let async1 = async
        |  raise "gg", "wp";
        |end async;
        |
        |let async2 = async
        |  return lambda();
        |end async;
        |
        |let async3 = async
        |  return 1;
        |end async;
        |
        |let res = await_any($async1, $async2, $async3);
        |""".stripMargin

    val lambda: Object =
      new (() => Int) {
        def apply(): Int = {
          Thread.sleep(1000)
          3
        }
      }
    val context = runMainVisitor(
      code,
      Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub", MutMap[String, Object]("lambda" -> lambda))
    )

    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1)
  }

  test("Test await_any all errors") {
    val code =
      """
        |let async1 = async
        |  raise "gg", "wp";
        |end async;
        |
        |let async2 = async
        |  raise "gg", "wp";
        |end async;
        |
        |let async3 = async
        |  raise "gg", "wp";
        |end async;
        |
        |let res = await_any($async1, $async2, $async3);
        |""".stripMargin

    val context = runMainVisitor(code)

    val res = context.getVar("res")
    assert(res.isInstanceOf[MException])
    assert(res.asInstanceOf[MException].getMessage == "wp")
  }

  test("Test closeEngine current") {
    val code =
      """
        |closeEngine();
                """.stripMargin
    class Other extends Engine with AutoCloseable {
      var closed = false
      override def name: String = "other"

      override def executeImpl(stmt: String, ctx: EngineContext): MType = ???

      override def executeFuncImpl(name: String,
                                   ctx: EngineContext,
                                   kwargs: Map[String, Object],
                                   params: MType*): MType = ???

      override def close(): Unit = { closed = true }
    }
    val engine = new Other
    val context = runMainVisitor(code, Context(MutMap("stub" -> engine), "stub"))

    assert(engine.closed)
  }

  test("Test closeEngine by name") {
    val code =
      """
        |closeEngine("stub");
                """.stripMargin
    class Other extends Engine with AutoCloseable {
      var closed = false
      override def name: String = "other"

      override def executeImpl(stmt: String, ctx: EngineContext): MType = ???

      override def executeFuncImpl(name: String,
                                   ctx: EngineContext,
                                   kwargs: Map[String, Object],
                                   params: MType*): MType = ???

      override def close(): Unit = { closed = true }
    }
    val engine = new Other
    val context = runMainVisitor(code, Context(MutMap("stub" -> engine), "stub"))

    assert(engine.closed)
  }
}
