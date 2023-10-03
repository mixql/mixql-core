package org.mixql.core.test.function

import org.mixql.core.context.Context
import org.mixql.core.context.mtype._
import org.mixql.core.engine.Engine
import org.mixql.core.logger.logInfo
import org.mixql.core.test.MainVisitorBaseTest
import org.mixql.core.test.engines.{CursorTest3, StubEngine}

import scala.collection.mutable

class FunctionsTestReturnType extends MainVisitorBaseTest {

  object SimpleFuncs {

    val simple_func_return_none = {
      new ((Context) => MNone) {

        override def apply(ctx: Context): MNone = {
          MNone.get()
        }
      }

    }

    val simple_func_return_cursor = {
      new ((Context) => MCursorBase) {

        override def apply(ctx: Context): MCursorBase = {
          new CursorTest3()
        }
      }

    }
  }

  val functions: mutable.Map[String, Any] = mutable.Map(
    "simple_func_return_none" -> SimpleFuncs.simple_func_return_none,
    "simple_func_return_cursor" -> SimpleFuncs.simple_func_return_cursor
  )

  test("Test simple function that return mixql type none") {
    val code =
      """
        |let foo = simple_func_return_none();
        |""".stripMargin

    val context = runMainVisitor(
      code, {
        Context(mutable.Map[String, Engine]("stub" -> new StubEngine), "stub", functions)
      }
    )

    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[MNone])
  }

  test("Test simple function that return mixql type cursor") {
    val code =
      """
        |let d_cursor = simple_func_return_cursor(); -- when defined, only when open is triggered
        |
        |open d_cursor; --executes expression here
        |
        |let arr = [];
        |
        |let res = null;
        |while $res != none do
        |   if $res != null then
        |       let arr = $arr + $res;
        |   end if
        |   let res = fetch d_cursor;
        |   -- print($res);
        |end while
        |
        |close d_cursor;
        |
        |print($arr);
        |""".stripMargin

    val context = runMainVisitor(
      code, {
        Context(mutable.Map[String, Engine]("stub" -> new StubEngine), "stub", functions)
      }
    )

    val arrRAW = context.getVar("arr")
    assert(arrRAW.isInstanceOf[MArray])

    val arr = arrRAW.asInstanceOf[MArray]

    assert(arr.size().getValue == 10)

    arr.getArr.foreach(value => {
      assert(value.isInstanceOf[MInt])
    })

  }
}
