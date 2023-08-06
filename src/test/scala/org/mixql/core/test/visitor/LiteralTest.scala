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

  test("Test if int not null") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = 5;
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

  test("Test if int not none") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = 5;
        |let flag = false;
        |if $res != none then
        |let flag = true;
        |end if
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("flag")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)
  }

  test("Test if none is none") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = none;
        |let flag = true;
        |if $res != none then
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

  test("Test if double is none") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = 5.3;
        |let flag = false;
        |if $res != none then
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

  test("Test if null not array") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = [5.3];
        |let flag = false;
        |if null != $res then
        |let flag = true;
        |end if
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("flag")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)
  }

  test("Test if array not none") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = [5.3];
        |let flag = false;
        |if $res != none then
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

  test("Test if map not none") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res =  {"5.3": 5.3};
        |let flag = false;
        |if $res != none then
        |let flag = true;
        |end if
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("flag")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)
  }

  test("Test if bool not null") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = true;
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

  test("Test if bool not none") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res =  true;
        |let flag = false;
        |if $res != none then
        |let flag = true;
        |end if
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("flag")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)
  }

  test("Test if string not null") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res = "true";
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

  test("Test if string not none") {
    import scala.collection.mutable.{Map => MutMap}

    val code =
      """
        |let res =  "true";
        |let flag = false;
        |if $res != none then
        |let flag = true;
        |end if
              """.stripMargin
    val context = runMainVisitor(code)

    val res1 = context.getVar("flag")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].getValue)
  }
}
