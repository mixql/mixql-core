package org.mixql.core.test.mtype

import org.mixql.core.context.Context
import org.mixql.core.context.mtype._
import org.mixql.core.engine.Engine
import org.mixql.core.test.MainVisitorBaseTest
import org.mixql.core.test.engines.StubEngine

import scala.collection.mutable.{Map => MutMap}

class MIntTest extends MainVisitorBaseTest {

  test("Test add for int") {
    Int.MaxValue
    val code =
      """
        |let res = 1 + 2;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == 3)
  }

  test("Test subtract for int") {
    Int.MaxValue
    val code =
      """
        |let res = 2 - 3;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == -1)
  }

  test("Test multiply for int") {
    Int.MaxValue
    val code =
      """
        |let res = 2 * 3;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == 6)
  }

  test("Test divide for int") {
    Int.MaxValue
    val code =
      """
        |let res = 10 / 5;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == 2)
  }

  test("Test non-integer division for int") {
    Int.MaxValue
    val code =
      """
        |let res = 10 / 3;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == 3)
  }

  test("Test > for int") {
    Int.MaxValue
    val code =
      """
        |let res = 10 > 3;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MBool])
    assert(res1.asInstanceOf[MBool].getValue == true)
  }

  test("Test >= for int") {
    Int.MaxValue
    val code =
      """
        |let res = 10 >= 10;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MBool])
    assert(res1.asInstanceOf[MBool].getValue)
  }

  test("Test < for int") {
    Int.MaxValue
    val code =
      """
        |let res = 10 < 10;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MBool])
    assert(!res1.asInstanceOf[MBool].getValue)
  }

  test("Test <= for int") {
    Int.MaxValue
    val code =
      """
        |let res = 10 <= 10;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MBool])
    assert(res1.asInstanceOf[MBool].getValue)
  }

  test("Test == for int") {
    Int.MaxValue
    val code =
      """
        |let res = 10 == 10;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MBool])
    assert(res1.asInstanceOf[MBool].getValue)
  }

  test("Test == for int (false case)") {
    Int.MaxValue
    val code =
      """
        |let res = 11 == 10;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MBool])
    assert(!res1.asInstanceOf[MBool].getValue)
  }

  test("Test != for int") {
    Int.MaxValue
    val code =
      """
        |let res = 11 <> 10;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MBool])
    assert(res1.asInstanceOf[MBool].getValue)
  }

  test("Test != for int (false case)") {
    Int.MaxValue
    val code =
      """
        |let res = 13 <> 13;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MBool])
    assert(!res1.asInstanceOf[MBool].getValue)
  }

  test("Test int to string") {
    Int.MaxValue
    val code =
      """
        |let res = 100 + "a";
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MString])
    assert(res1.asInstanceOf[MString].getValue == "100a")
  }

  test("Test cast int to string") {
    Int.MaxValue
    val code =
      """
        |let res = cast(1 as string);
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MString])
    assert(res1.asInstanceOf[MString].getValue == "1")
  }

  test("Test maxInt") {
    Int.MaxValue
    val code =
      """
        |let res = 2147483647;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == 2147483647)
  }

  test("Test maxLong") {
    Int.MaxValue
    val code =
      """
        |let res = 9223372036854775807;
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == 9223372036854775807L)
  }

  ignore("Test big integer") {
    Int.MaxValue
    val code =
      """
        |let res = 10000000000000000000 + "";
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res")
    assert(res1.isInstanceOf[MString])
    assert(res1.asInstanceOf[MString].getValue == "10000000000000000000")
  }

  test("Test getLong from function") {
    val code =
      """
        let res = getInt();
                """.stripMargin
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    val getLong =
      new (() => Long) {
        override def apply(): Long = {
          42L
        }
      }
    context.addFunction("getint", getLong)
    runMainVisitor(code, context)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 42)
  }

  test("Test getInt from function") {
    val code =
      """
        let res = getInt();
                """.stripMargin
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    val getInt =
      new (() => Int) {
        override def apply(): Int = {
          42
        }
      }
    context.addFunction("getint", getInt)
    runMainVisitor(code, context)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 42)
  }

  test("Test getInt from function with int param") {
    val code =
      """
        let res = getInt(1234);
                """.stripMargin
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    val getInt =
      new (Int => Int) {
        override def apply(p1: Int): Int = {
          p1.toInt
        }
      }
    context.addFunction("getint", getInt)
    runMainVisitor(code, context)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 1234)
  }

  test("Test getInt from function with long param") {
    val code =
      """
        let res = getInt(123);
                """.stripMargin
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    val getInt =
      new (Long => Int) {
        override def apply(p1: Long): Int = {
          p1.toInt
        }
      }
    context.addFunction("getint", getInt)
    runMainVisitor(code, context)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 123)
  }

  test("Test getInt from function with mixed param") {
    val code =
      """
        let res = getInt(55,45);
                """.stripMargin
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    val getInt =
      new ((Long, Int) => Int) {
        override def apply(p1: Long, p2: Int): Int = {
          (p1 + p2).toInt
        }
      }
    context.addFunction("getint", getInt)
    runMainVisitor(code, context)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 100)
  }

  test("Test getInt with long variable args length") {
    val code =
      """
        let res = getInt(1,2,3,4);
                """.stripMargin
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    val variableArgIntFunc =
      new Object {
        def apply(ints: Long*): Long = ints.reduce((a, b) => a.toInt + b.toInt)
      }
    context.addFunction("getint", variableArgIntFunc)
    runMainVisitor(code, context)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 10)
  }

  test("Test getInt with int variable args length") {
    val code =
      """
        let res = getInt(1,2,3,4);
                """.stripMargin
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    val variableArgIntFunc =
      new Object {
        def apply(ints: Int*): Int = ints.reduce((a, b) => a.toInt + b.toInt)
      }
    context.addFunction("getint", variableArgIntFunc)
    runMainVisitor(code, context)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 10)
  }
}
