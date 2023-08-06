package org.mixql.core.test.visitor

import org.mixql.core.context.Context
import org.mixql.core.context.gtype._
import org.mixql.core.engine.Engine
import org.mixql.core.test.MainVisitorBaseTest
import org.mixql.core.test.engines.{CursorTestEngine1, CursorTestEngine2}

class CursorTest extends MainVisitorBaseTest {
  test("Test cursor is array, fetch, open, close") {
    val code =
      """
        |let d_cursor = cursor is [TRUE, [TRUE, "gg", 12], 12]; --does not execute when defined,
        |-- only when open is triggered
        |
        |open d_cursor; --executes expression here
        |
        |let res1 = fetch d_cursor;
        |print("first element is $res1");
        |
        |let res2 = fetch d_cursor;
        |print("second element is $res2");
        |
        |let res3 = fetch d_cursor;
        |print("third element is $res3");
        |
        |let res4 = fetch d_cursor;
        |print("fourth element is $res4");
        |
        |let res5 = fetch d_cursor;
        |print("fifth element is $res5");
        |
        |close d_cursor;
        |
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)

    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[array])
    assert(res2.asInstanceOf[array].toString == "[true, \"gg\", 12]")

    val res3 = context.getVar("res3")
    assert(res3.isInstanceOf[gInt])
    assert(res3.asInstanceOf[gInt].getValue == 12)

    val res4 = context.getVar("res4")
    assert(res4.isInstanceOf[none])
    val res5 = context.getVar("res5")
    assert(res5.isInstanceOf[none])
  }

  test("Test cursor is map, fetch, open, close") {
    val code =
      """
        |let d_cursor = cursor is {"fb": TRUE,
        |   "sarr": [TRUE, "gg", 12], "sgint": 12
        |}; --does not execute when defined, only when open is triggered
        |
        |open d_cursor; --executes expression here
        |
        |let res1 = fetch d_cursor;
        |print("first element is $res1");
        |
        |let res2 = fetch d_cursor;
        |print("second element is $res2");
        |
        |let res3 = fetch d_cursor;
        |print("third element is $res3");
        |
        |let res4 = fetch d_cursor;
        |print("fourth element is $res4");
        |
        |let res5 = fetch d_cursor;
        |print("fifth element is $res5");
        |
        |close d_cursor;
        |
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("res1")
    assert(res1.isInstanceOf[array])
    assert(
      res1.asInstanceOf[array].toString == "[\"fb\", true]" ||
        res1.asInstanceOf[array].toString == "[\"sarr\", [true, \"gg\", 12]]" ||
        res1.asInstanceOf[array].toString == "[\"sgint\", 12]"
    )

    val res2 = context.getVar("res2")
    assert(res2.isInstanceOf[array])
    assert(
      res2.asInstanceOf[array].toString == "[\"fb\", true]" ||
        res2.asInstanceOf[array].toString == "[\"sarr\", [true, \"gg\", 12]]" ||
        res2.asInstanceOf[array].toString == "[\"sgint\", 12]"
    )

    val res3 = context.getVar("res3")
    assert(res3.isInstanceOf[array])
    assert(
      res3.asInstanceOf[array].toString == "[\"fb\", true]" ||
        res3.asInstanceOf[array].toString == "[\"sarr\", [true, \"gg\", 12]]" ||
        res3.asInstanceOf[array].toString == "[\"sgint\", 12]"
    )

    val res4 = context.getVar("res4")
    assert(res4.isInstanceOf[none])
    val res5 = context.getVar("res5")
    assert(res5.isInstanceOf[none])
  }

  test("Test cursor will not fetch without open") {
    val code =
      """
        |let d_cursor = cursor is {"fb": TRUE,
        |   "sarr": [TRUE, "gg", 12], "sgint": 12
        |}; --does not execute when defined, only when open is triggered
        |
        |
        |let res1 = fetch d_cursor;
              """.stripMargin
    assertThrows[Exception] {
      val context = runMainVisitor(code)
    }
  }

  test("Test cursor with engine that does not return cursor") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let d_cursor = cursor is (select * from table_a);--does not execute
        |-- when defined, only when open is triggered
        |
        |open d_cursor; --executes expression here
        |
        |let arr = [];
        |
        |for i in 1..20 loop
        |     let arr = $arr + fetch d_cursor;
        |end loop
        |
        |print("arr is $arr");
        |
        |close d_cursor;
              """.stripMargin
    val context = runMainVisitor(
      code,
      new Context(MutMap[String, Engine]("CursorTestEngine1" -> new CursorTestEngine1), "CursorTestEngine1")
    )

    val res1 = context.getVar("arr")
    assert(res1.isInstanceOf[array])
    val arr1 = res1.asInstanceOf[array]
    val arr1Size = arr1.size().getValue
    assert(arr1Size == 20)

    for (i <- 0 to arr1Size - 1) {
      if (i < 7)
        assert(arr1.apply(new gInt(i)).isInstanceOf[gInt])
      else
        assert(arr1.apply(new gInt(i)).isInstanceOf[none])
    }
  }

  test("Test cursor with engine that return cursor in while") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let d_cursor = cursor is (select * from table_a);--does not execute
        |-- when defined, only when open is triggered
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
              """.stripMargin
    val context = runMainVisitor(
      code,
      new Context(MutMap[String, Engine]("CursorTestEngine2" -> new CursorTestEngine2), "CursorTestEngine2")
    )

    val res1 = context.getVar("arr")
    assert(res1.isInstanceOf[array])
    val arr1 = res1.asInstanceOf[array]
    val arr1Size = arr1.size().getValue
    assert(arr1Size == 10)

    for (i <- 0 until arr1Size) {
      assert(arr1.apply(new gInt(i)).isInstanceOf[gInt])
    }
  }

  test("Test cursor with for in cursor var") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let d_cursor = cursor is (select * from table_a);--does not execute
        |-- when defined, only when open is triggered
        |
        |open d_cursor; --executes expression here
        |
        |let arr = [];
        |
        |let res = null;
        |-- iterates while nothing is not got
        |for res in $d_cursor loop
        |   if $res != null then
        |       let arr = $arr + $res;
        |   end if
        |   print($res);
        |end loop
        |
        |close d_cursor;
              """.stripMargin
    val context = runMainVisitor(
      code,
      new Context(MutMap[String, Engine]("CursorTestEngine2" -> new CursorTestEngine2), "CursorTestEngine2")
    )

    val res1 = context.getVar("arr")
    assert(res1.isInstanceOf[array])
    val arr1 = res1.asInstanceOf[array]
    val arr1Size = arr1.size().getValue
    assert(arr1Size == 10)

    for (i <- 0 until arr1Size) {
      assert(arr1.apply(new gInt(i)).isInstanceOf[gInt])
    }
  }

  test("Test cursor with for in cursor is array") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let arr = [];
        |
        |let res = null;
        |
        |for res in cursor is ['a', 'b', 'c'] loop --anonymous cursor, will be opened and closed after
        |   -- leaving for scope
        |
        |   if $res != null then
        |       let arr = $arr + $res;
        |   end if
        |   print($res);
        |end loop
        |
              """.stripMargin
    val context = runMainVisitor(
      code,
      new Context(MutMap[String, Engine]("CursorTestEngine2" -> new CursorTestEngine2), "CursorTestEngine2")
    )

    val res1 = context.getVar("arr")
    assert(res1.isInstanceOf[array])
    val arr1 = res1.asInstanceOf[array]
    val arr1Size = arr1.size().getValue
    assert(arr1Size == 3)

    assert(arr1.apply(new gInt(0)).isInstanceOf[string])
    assert(arr1.apply(new gInt(0)).asInstanceOf[string].toString == "a")
    assert(arr1.apply(new gInt(1)).isInstanceOf[string])
    assert(arr1.apply(new gInt(1)).asInstanceOf[string].toString == "b")
    assert(arr1.apply(new gInt(2)).isInstanceOf[string])
    assert(arr1.apply(new gInt(2)).asInstanceOf[string].toString == "c")
  }

  test("Test cursor with for in cursor select from *") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let arr = [];
        |
        |let res = null;
        |
        |for res in cursor is (select * from table_a) loop --anonymous cursor, will be opened and closed after
        |   -- leaving for scope
        |
        |   if $res != null then
        |       let arr = $arr + $res;
        |   end if
        |   print($res);
        |end loop
              """.stripMargin
    val context = runMainVisitor(
      code,
      new Context(MutMap[String, Engine]("CursorTestEngine2" -> new CursorTestEngine2), "CursorTestEngine2")
    )

    val res1 = context.getVar("arr")
    assert(res1.isInstanceOf[array])
    val arr1 = res1.asInstanceOf[array]
    val arr1Size = arr1.size().getValue
    assert(arr1Size == 10)

    for (i <- 0 until arr1Size) {
      assert(arr1.apply(new gInt(i)).isInstanceOf[gInt])
    }
  }
}
