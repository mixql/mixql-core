package org.mixql.core.test.visitor

import org.mixql.core.context.gtype.bool
import org.mixql.core.test.MainVisitorBaseTest

class LiteralTest extends MainVisitorBaseTest {

  test("Test if double not null") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = 5.3;
        |let flag = false;
        |if $res != null then
        |let flag = true;
        |end if
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("flag")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)
  }

  test("Test if nothing is nothing") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = nothing;
        |let flag = true;
        |if $res != nothing then
        |   let flag = false;
        |end if
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("flag")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)
  }

  test("Test null if null") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = null;
        |let flag = true;
        |if $res != null then
        |   let flag = false;
        |end if
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("flag")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)
  }

  test("Test if double is nothing") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = 5.3;
        |let flag = false;
        |if $res != nothing then
        |let flag = true;
        |end if
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("flag")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)
  }

  test("Test if array not null") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = [5.3];
        |let flag = false;
        |if $res != null then
        |let flag = true;
        |end if
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("flag")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)
  }

  test("Test if array not nothing") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = [5.3];
        |let flag = false;
        |if $res != nothing then
        |let flag = true;
        |end if
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("flag")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)
  }

  test("Test if map not null") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = {"5.3": 5.3};
        |let flag = false;
        |if $res != null then
        |let flag = true;
        |end if
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("flag")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)
  }

  test("Test if map not nothing") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res =  {"5.3": 5.3};
        |let flag = false;
        |if $res != nothing then
        |let flag = true;
        |end if
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("flag")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)
  }

}
