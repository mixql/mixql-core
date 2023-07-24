package org.mixql.core.test.visitor

import org.mixql.core.context.gtype._
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
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "")
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
    assert(res.isInstanceOf[gDouble])
    assert(res.asInstanceOf[gDouble].getValue == 0.5)
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
    assert(res.isInstanceOf[bool])
    assert(res.asInstanceOf[bool].getValue == false)

    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue == true)
  }

  test("Test string expression") {
    val code =
      """
        |let res = 'one' || 2 || true;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "one2true")
  }

  test("Test case then expression") {
    val code =
      """
        |let res = case when 2 >= 1 then -1.5 else 'false' end;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[gDouble])
    assert(res.asInstanceOf[gDouble].getValue == -1.5)
  }

  test("Test case else expression") {
    val code =
      """
        |let res = case when 2 <= 1 then true else 'false' end;
        |let check_case = case when 1 > 2 then 12 when 1 < 2 then 13 else '12g' end;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "false")
  }

  test("Test case switch expression") {
    val code =
      """
        |let sw = "when";
        |let res = case $sw when "when" then true else 'false' end;
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[bool])
    assert(res.asInstanceOf[bool].getValue == true)
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
    assert(res.isInstanceOf[gDouble])
    assert(res.asInstanceOf[gDouble].getValue == 8.5)
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
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "57.5")
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
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "5.5gg")
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
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].getValue == "truegg")
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
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue == true)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[bool])
    assert(res2.asInstanceOf[bool].getValue == false)
    val res3 = context.getVar("res3")
    assert(res3.isInstanceOf[bool])
    assert(res3.asInstanceOf[bool].getValue == true)
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
    assert(res1.isInstanceOf[gInt])
    assert(res1.asInstanceOf[gInt].getValue == 1)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[gInt])
    assert(res2.asInstanceOf[gInt].getValue == 12)
    val res3 = context.getVar("res3")
    assert(res3.isInstanceOf[gInt])
    assert(res3.asInstanceOf[gInt].getValue == 1)
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
    assert(res1.isInstanceOf[gDouble])
    assert(res1.asInstanceOf[gDouble].getValue == 1.0)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[gDouble])
    assert(res2.asInstanceOf[gDouble].getValue == 17.9)
    val res3 = context.getVar("res3")
    assert(res3.isInstanceOf[gDouble])
    assert(res3.asInstanceOf[gDouble].getValue == 11.0)
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
    assert(res1.isInstanceOf[string])
    assert(res1.asInstanceOf[string].getValue == "true")
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[string])
    assert(res2.asInstanceOf[string].getValue == "17.9")
    val res3 = context.getVar("res3")
    assert(res3.isInstanceOf[string])
    assert(res3.asInstanceOf[string].getValue == "11")
  }

  test("Test array literal") {
    val code =
      """
        |let res = [TRUE, "gg", 12];
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[array])
    val arr = res.asInstanceOf[array]
    assert(
      arr.apply(new gInt(0)).Equal(new bool(true)).asInstanceOf[bool].getValue)
    assert(
      arr.apply(new gInt(1)).Equal(new string("gg")).asInstanceOf[bool]
        .getValue)
    assert(
      arr.apply(new gInt(2)).Equal(new gInt(12)).asInstanceOf[bool].getValue)
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
    assert(res1.isInstanceOf[bool])
    assert(
      res1.asInstanceOf[bool].Equal(new bool(true)).asInstanceOf[bool].getValue)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[bool])
    assert(
      (
        res2.asInstanceOf[bool].Equal(new bool(false)).asInstanceOf[bool]
          .getValue
      ))
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
    assert(res.isInstanceOf[array])
    val arr = res.asInstanceOf[array]
    logDebug("arr size: " + arr.size)
    assert(arr.size.Equal(new gInt(5)).asInstanceOf[bool].getValue)
    assert(arr(new gInt(0)).Equal(new gInt(1)).asInstanceOf[bool].getValue)
    assert(
      arr(new gInt(4)).Equal(new string("last")).asInstanceOf[bool].getValue)
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
    assert(res.isInstanceOf[array])
    val arr = res.asInstanceOf[array]
    assert(arr.size.Equal(new gInt(2)).asInstanceOf[bool].getValue)
    assert(arr(new gInt(0)).Equal(new bool(true)).asInstanceOf[bool].getValue)
    assert(arr(new gInt(1)).Equal(new bool(false)).asInstanceOf[bool].getValue)
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
    assert(res1.isInstanceOf[array])
    val arr = res1.asInstanceOf[array]
    assert(arr.size.Equal(new gInt(2)).asInstanceOf[bool].getValue)
    assert(arr(new gInt(0)).Equal(new gInt(1)).asInstanceOf[bool].getValue)
    assert(arr(new gInt(1)).Equal(new gInt(2)).asInstanceOf[bool].getValue)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[gInt])
    assert(res2.asInstanceOf[gInt].getValue == 1)
  }

  test("Test call lambda") {
    val code =
      """
        |let mysumm = (x, y, z) -> begin $x + $y + $z; end;
        |let res = mysumm(1, 2, 3);
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[gInt])
    val i = res.asInstanceOf[gInt]
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
    assert(res2.isInstanceOf[bool])
    assert(
      res2.asInstanceOf[bool].Equal(new bool(false)).asInstanceOf[bool]
        .getValue)
  }

  test("Test map literal") {
    val code =
      """
        |let res = {1: 1, "1": 2};
        |print($res);
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[map])
    val mapa = res.asInstanceOf[map]
    assert(mapa.size.getValue == 2)
    assert(mapa(new gInt(1)).Equal(new gInt(1)).asInstanceOf[bool].getValue)
    assert(mapa(new string("1")).Equal(new gInt(2)).asInstanceOf[bool].getValue)
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
    assert(res1.isInstanceOf[gInt])
    assert(
      res1.asInstanceOf[gInt].Equal(new gInt(1)).asInstanceOf[bool].getValue)
    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[bool])
    assert(
      res2.asInstanceOf[bool].Equal(new bool(false)).asInstanceOf[bool]
        .getValue)
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
    assert(res2.isInstanceOf[bool])
    assert(
      res2.asInstanceOf[bool].Equal(new bool(false)).asInstanceOf[bool]
        .getValue)
  }

  test("Test call engine specific function") {
    val code =
      """
        |let res = getnum();
                """.stripMargin
    val context = runMainVisitor(code)
    val res = context.getVar("res")
    assert(res.isInstanceOf[gInt])
    assert(res.asInstanceOf[gInt].getValue == 42)
  }

  test("Test call function uses context") {
    val code =
      """
        |let a = 32;
        |let res = testcontext("gg", 10);
                """.stripMargin
    val context =
      new Context(MutMap[String, Engine]("stub" -> (new StubEngine)), "stub")
    val testContext =
      new ((String, Int, Context) => Int) {
        override def apply(str: String, num: Int, context: Context): Int = {
          val a = context.getVar("a")
          a.asInstanceOf[gInt].getValue + num
        }
      }
    context.addFunction("testcontext", testContext)
    runMainVisitor(code, context)
    val res = context.getVar("res")
    assert(res.isInstanceOf[gInt])
    assert(res.asInstanceOf[gInt].getValue == 42)
  }

  test("Test call function params by names") {
    val code =
      """
        |let a = 32;
        |let res = testcontext(num = 10, str="gg");
                """.stripMargin
    val context =
      new Context(MutMap[String, Engine]("stub" -> (new StubEngine)), "stub")
    val testContext =
      new ((String, Int, Context) => Int) {
        override def apply(str: String, num: Int, context: Context): Int = {
          val a = context.getVar("a")
          a.asInstanceOf[gInt].getValue + num
        }
      }
    context.addFunction("testcontext", testContext)
    runMainVisitor(code, context)
    val res = context.getVar("res")
    assert(res.isInstanceOf[gInt])
    assert(res.asInstanceOf[gInt].getValue == 42)
  }
}
