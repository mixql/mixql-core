package org.mixql.core.test.visitor

import org.mixql.core.test.MainVisitorBaseTest
import org.mixql.core.test.engines.StubEngine
import org.mixql.core.test.tag.Interpolation

@Interpolation
class OtherInterpolationTest extends MainVisitorBaseTest {

  test("Test any statement variable interpolation") {
    val code =
      """
        |let a.a = 10;
        |select $a.a from table where column > 10;
                """.stripMargin
    val context = runMainVisitor(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(query.dequeue() == "select 10 from table where column > 10")
  }

  test("Test other statement expression interpolation") {
    val code =
      """
        |let a = 10;
        |select ${$a - 3} from table where column > 10;
                """.stripMargin
    val context = runMainVisitor(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(query.dequeue() == "select 7 from table where column > 10")
  }

  test("Test other statement string interpolation") {
    val code =
      """
        |let a = 10;
        |let b = 'some str';
        |select '\$${$a || ' df;df $b'}\\' from table where column > 10;
                """.stripMargin
    val context = runMainVisitor(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(
      query.dequeue() ==
        "select '$10 df;df some str\\' from table where column > 10"
    )
  }

  test("Test any statement with curly brackets") {
    val code =
      """
        |select \$ { dfsg } { 15 * 10 } {\\} where > 10;
                """.stripMargin
    val context = runMainVisitor(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(query.dequeue() == "select $ { dfsg } { 15 * 10 } {\\} where > 10")
  }

  test("Test any statement with SEMICOLON") {
    val code =
      """
        |select \; { dfsg } { 15 * 10 } {\\} where > 10;
                """.stripMargin
    val context = runMainVisitor(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(query.dequeue() == "select ; { dfsg } { 15 * 10 } {\\} where > 10")
  }

  test("Test any statement with \\n") {
    val code =
      """
        |select \n { dfsg } { 15 * 10 } {\\} where > 10;
                """.stripMargin
    val context = runMainVisitor(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(query.dequeue() == "select \\n { dfsg } { 15 * 10 } {\\} where > 10")
  }

  test("Test any statement with trigger") {
    val code =
      """
        |CREATE TRIGGER actor_trigger_ai AFTER INSERT ON actor
        | BEGIN
        |  UPDATE actor SET last_update = DATETIME('NOW')  WHERE rowid = new.rowid\;
        | END
        |;
                """.stripMargin
    val context = runMainVisitor(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(
      query.dequeue() ==
        """|CREATE TRIGGER actor_trigger_ai AFTER INSERT ON actor
        | BEGIN
        |  UPDATE actor SET last_update = DATETIME('NOW')  WHERE rowid = new.rowid;
        | END""".stripMargin
    )
  }

}
