package org.mixql.core.test.visitor

import org.mixql.core.context.mtype._
import org.mixql.core.test.MainVisitorBaseTest
import org.mixql.core.test.engines.StubEngine
import org.mixql.core.context.Context
import org.mixql.core.engine.Engine
import org.mixql.core.logger.logDebug

import scala.collection.mutable.{Map => MutMap}

class ExpressionTest extends MainVisitorBaseTest {

  test("Test empty string") {
    val code =
      """
        |let res = "";
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "")
  }

  test("Test arithmetic expression") {
    val code =
      """
        |let a = 0.5;
        |let b = 1.5;
        |let res = (($a + $b) * $a) / 2;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MDouble])
    assert(res.asInstanceOf[MDouble].getValue == 0.5)
  }

  test("Test bool expression") {
    val code =
      """
        |let a = 12;
        |let res = $a > 11 and $a <> 12;
        |let res1 = $a > 11 or $a != 12;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MBool])
    assert(res.asInstanceOf[MBool].getValue == false)

    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MBool])
    assert(res1.asInstanceOf[MBool].getValue == true)
  }

  test("Test string expression") {
    val code =
      """
        |let res = 'one' || 2 || true;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "one2true")
  }

  test("Test case then expression") {
    val code =
      """
        |let res = case when 2 >= 1 then -1.5 else 'false' end;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MDouble])
    assert(res.asInstanceOf[MDouble].getValue == -1.5)
  }

  test("Test case else expression") {
    val code =
      """
        |let res = case when 2 <= 1 then true else 'false' end;
        |let check_case = case when 1 > 2 then 12 when 1 < 2 then 13 else '12g' end;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "false")
  }

  test("Test case switch expression") {
    val code =
      """
        |let sw = "when";
        |let res = case $sw when "when" then true else 'false' end;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MBool])
    assert(res.asInstanceOf[MBool].getValue == true)
  }

  test("Test some java code in expression bracketed") {
    val code =
      """
        |(
        |if ( true ) {
        |    System.out.println ( 12 ) ;
        |} else {
        |    System.out.println ( 9 ) ;
        |}
        |);
                """.stripMargin
    val context = runMainVisitor(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(query.dequeue() == """
                                |if ( true ) {
                                |    System.out.println ( 12 ) ;
                                |} else {
                                |    System.out.println ( 9 ) ;
                                |}
                """.stripMargin.trim)
  }

  test("Test complex expression") {
    val code =
      """
        |let a = 5;
        |let b = 7.5;
        |let res = (10 - $b) + (3.5 - $a / 2) * 4;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MDouble])
    assert(res.asInstanceOf[MDouble].getValue == 8.5)
  }

  test("Test int + string") {
    val code =
      """
        |let a = 5;
        |let b = "7.5";
        |let res = $a + $b;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "57.5")
  }

  test("Test double + string") {
    val code =
      """
        |let a = 5.5;
        |let b = "gg";
        |let res = $a + $b;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "5.5gg")
  }

  test("Test bool + string") {
    val code =
      """
        |let a = true;
        |let b = "gg";
        |let res = $a + $b;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MString])
    assert(res.asInstanceOf[MString].getValue == "truegg")
  }

  test("Test cast to bool") {
    val code =
      """
        |let a1 = "true";
        |let a2 = 0;
        |let a3 = 1.0;
        |let res1 = cast($a1 as bool);
        |let res2 = cast($a2 as bool);
        |let res3 = cast($a3 as bool);
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MBool])
    assert(res1.asInstanceOf[MBool].getValue == true)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[MBool])
    assert(res2.asInstanceOf[MBool].getValue == false)
    val res3 = context.getVar("res3")
    assert(res3.isInstanceOf[MBool])
    assert(res3.asInstanceOf[MBool].getValue == true)
  }

  test("Test cast to int") {
    val code =
      """
        |let a1 = true;
        |let a2 = "12";
        |let a3 = 1.5;
        |let res1 = cast($a1 as int);
        |let res2 = cast($a2 as int);
        |let res3 = cast($a3 as int);
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].getValue == 1)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[MInt])
    assert(res2.asInstanceOf[MInt].getValue == 12)
    val res3 = context.getVar("res3")
    assert(res3.isInstanceOf[MInt])
    assert(res3.asInstanceOf[MInt].getValue == 1)
  }

  test("Test cast to double") {
    val code =
      """
        |let a1 = true;
        |let a2 = "17.9";
        |let a3 = 11;
        |let res1 = cast($a1 as double);
        |let res2 = cast($a2 as double);
        |let res3 = cast($a3 as double);
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MDouble])
    assert(res1.asInstanceOf[MDouble].getValue == 1.0)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[MDouble])
    assert(res2.asInstanceOf[MDouble].getValue == 17.9)
    val res3 = context.getVar("res3")
    assert(res3.isInstanceOf[MDouble])
    assert(res3.asInstanceOf[MDouble].getValue == 11.0)
  }

  test("Test cast to string") {
    val code =
      """
        |let a1 = TRUE;
        |let a2 = 17.9;
        |let a3 = 11;
        |let res1 = cast($a1 as string);
        |let res2 = cast($a2 as string);
        |let res3 = cast($a3 as string);
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MString])
    assert(res1.asInstanceOf[MString].getValue == "true")
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[MString])
    assert(res2.asInstanceOf[MString].getValue == "17.9")
    val res3 = context.getVar("res3")
    assert(res3.isInstanceOf[MString])
    assert(res3.asInstanceOf[MString].getValue == "11")
  }

  test("Test array literal") {
    val code =
      """
        |let res = [TRUE, "gg", 12];
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MArray])
    val arr = res.asInstanceOf[MArray]
    assert(arr.apply(new MInt(0)).Equal(new MBool(true)).asInstanceOf[MBool].getValue)
    assert(arr.apply(new MInt(1)).Equal(new MString("gg")).asInstanceOf[MBool].getValue)
    assert(arr.apply(new MInt(2)).Equal(new MInt(12)).asInstanceOf[MBool].getValue)
  }

  test("Test array get/set by index") {
    val code =
      """
        |let arr = [TRUE, "gg", 12];
        |let res1 = $arr[0];
        |let arr[0] = false;
        |let res2 = $arr[0];
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MBool])
    assert(res1.asInstanceOf[MBool].Equal(new MBool(true)).asInstanceOf[MBool].getValue)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[MBool])
    assert(
      (
        res2.asInstanceOf[MBool].Equal(new MBool(false)).asInstanceOf[MBool].getValue
      )
    )
  }

  test("Test append and prepend value to array") {
    val code =
      """
        |let res = [TRUE, "gg", 12];
        |let res = $res + "last";
        |let res = 1 + $res;
        |print($res);
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MArray])
    val arr = res.asInstanceOf[MArray]
    logDebug("arr size: " + arr.size)
    assert(arr.size.Equal(new MInt(5)).asInstanceOf[MBool].getValue)
    assert(arr(new MInt(0)).Equal(new MInt(1)).asInstanceOf[MBool].getValue)
    assert(arr(new MInt(4)).Equal(new MString("last")).asInstanceOf[MBool].getValue)
  }

  test("Test concat 2 arrays") {
    val code =
      """
        |let a1 = [TRUE];
        |let a2 = [false];
        |let res = $a1 + $a2;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MArray])
    val arr = res.asInstanceOf[MArray]
    assert(arr.size.Equal(new MInt(2)).asInstanceOf[MBool].getValue)
    assert(arr(new MInt(0)).Equal(new MBool(true)).asInstanceOf[MBool].getValue)
    assert(arr(new MInt(1)).Equal(new MBool(false)).asInstanceOf[MBool].getValue)
  }

  test("Test index priority") {
    val code =
      """
        |let arr1 = [1];
        |let arr2 = [2];
        |let res1 = $arr1 + $arr2[0];
        |let res2 = ($arr1 + $arr2)[0];
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MArray])
    val arr = res1.asInstanceOf[MArray]
    assert(arr.size.Equal(new MInt(2)).asInstanceOf[MBool].getValue)
    assert(arr(new MInt(0)).Equal(new MInt(1)).asInstanceOf[MBool].getValue)
    assert(arr(new MInt(1)).Equal(new MInt(2)).asInstanceOf[MBool].getValue)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[MInt])
    assert(res2.asInstanceOf[MInt].getValue == 1)
  }

  test("Test call lambda") {
    val code =
      """
        |let mysumm = (x, y, z) -> begin $x + $y + $z; end;
        |let res = mysumm(1, 2, 3);
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    val i = res.asInstanceOf[MInt]
    assert(i.getValue == 6)
  }

  test("Test array pack/unpack in lambda") {
    val code =
      """
        |let arr = [TRUE, "gg", 12];
        |let foo = (x) -> begin
        |  let x[0] = false;
        |  return $x;
        |end;
        |let arr1 = foo($arr);
        |let res = $arr1[0];
                """.stripMargin
    val context = runMainVisitor(code)
    val res2 = context.getVar("res")
    assert(res2.isInstanceOf[MBool])
    assert(res2.asInstanceOf[MBool].Equal(new MBool(false)).asInstanceOf[MBool].getValue)
  }

  test("Test map literal") {
    val code =
      """
        |let res = {1: 1, "1": 2};
        |print($res);
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MMap])
    val mapa = res.asInstanceOf[MMap]
    assert(mapa.size.getValue == 2)
    assert(mapa(new MInt(1)).Equal(new MInt(1)).asInstanceOf[MBool].getValue)
    assert(mapa(new MString("1")).Equal(new MInt(2)).asInstanceOf[MBool].getValue)
  }

  test("Test map get/set by index") {
    val code =
      """
        |let mapa = {1.1: 1, "1.1": 2};
        |let res1 = $mapa[1.1];
        |let mapa[1.1] = false;
        |let res2 = $mapa[1.1];
        |print("mapa is: $mapa");
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[MInt])
    assert(res1.asInstanceOf[MInt].Equal(new MInt(1)).asInstanceOf[MBool].getValue)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[MBool])
    assert(res2.asInstanceOf[MBool].Equal(new MBool(false)).asInstanceOf[MBool].getValue)
  }

  test("Test map pack/unpack in lambda") {
    val code =
      """
        |let mapa = {1.1: 1, "1.1": 2};
        |let foo = (x) -> begin
        |  let x[1.1] = false;
        |  return $x;
        |end;
        |let mapa1 = foo($mapa);
        |let res = $mapa1[1.1];
        |print($res);
                """.stripMargin
    val context = runMainVisitor(code)
    val res2 = context.getVar("res")
    assert(res2.isInstanceOf[MBool])
    assert(res2.asInstanceOf[MBool].Equal(new MBool(false)).asInstanceOf[MBool].getValue)
  }

  test("Test call engine specific function") {
    val code =
      """
        |let res = getnum();
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 42)
  }

  test("Test call function uses context") {
    val code =
      """
        |let a = 32;
        |let res = testcontext("gg", 10);
                """.stripMargin
    val context = Context(MutMap[String, Engine]("stub" -> (new StubEngine)), "stub")
    val testContext =
      new ((String, Long, Context) => Long) {
        override def apply(str: String, num: Long, context: Context): Long = {
          val a = context.getVar("a")
          a.asInstanceOf[MInt].getValue + num
        }
      }
    context.addFunction("testcontext", testContext)
    runMainVisitor(code, context)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 42)
  }

  test("Test call function params by names") {
    val code =
      """
        |let a = 32;
        |let res = testcontext(num = 10, str="gg");
                """.stripMargin
    val context = Context(MutMap[String, Engine]("stub" -> (new StubEngine)), "stub")
    val testContext =
      new ((String, Long, Context) => Long) {
        override def apply(str: String, num: Long, context: Context): Long = {
          val a = context.getVar("a")
          a.asInstanceOf[MInt].getValue + num
        }
      }
    context.addFunction("testcontext", testContext)
    runMainVisitor(code, context)
    val res = context.getVar("res")
    assert(res.isInstanceOf[MInt])
    assert(res.asInstanceOf[MInt].getValue == 42)
  }
}
