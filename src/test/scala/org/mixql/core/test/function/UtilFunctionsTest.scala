package org.mixql.core.test.function

import org.mixql.core.context.mtype._
import org.mixql.core.test.MainVisitorBaseTest
import org.mixql.core.engine.Engine
import org.mixql.core.context.{Context, EngineContext}
import org.mixql.core.test.engines.StubEngine

import scala.collection.mutable.{Map => MutMap}

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
    val lambda: Any =
      new (() => Int) {
        def apply(): Int = throw new Exception("await")
      }
    val context = runMainVisitor(
      code,
      Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub", MutMap[String, Any]("lambda" -> lambda))
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
}
