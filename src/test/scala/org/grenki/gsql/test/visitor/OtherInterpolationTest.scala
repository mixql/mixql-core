package org.grenki.gsql.test.visitor

import org.grenki.gsql.test.MainVisitorBaseTest
import org.grenki.gsql.test.stub.StubEngine
import org.grenki.gsql.test.tag.Interpolation

@Interpolation
class OtherInterpolationTest extends MainVisitorBaseTest {
  test("Test any statement variable interpolation") {
    val code =
      """
        |set a.a = 10;
        |select $a.a from table where column > 10;
                """.stripMargin
    val context = runMainVisitor(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(query.dequeue() == "select 10 from table where column > 10")
  }

  test("Test other statement expression interpolation") {
    val code =
      """
        |set a = 10;
        |select ${$a - 3} from table where column > 10;
                """.stripMargin
    val context = runMainVisitor(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(query.dequeue() == "select 7 from table where column > 10")
  }

  test("Test other statement string interpolation") {
    val code =
      """
        |set a = 10;
        |set b = 'some str';
        |select '\$${$a || ' df;df $b'}\\' from table where column > 10;
                """.stripMargin
    val context = runMainVisitor(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(
      query
        .dequeue() == "select '$10 df;df some str\\' from table where column > 10"
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
}
