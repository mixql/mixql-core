package org.mixql.core.test.function

import org.mixql.core.context.mtype._
import org.mixql.core.test.MainVisitorBaseTest

class StringFunctionsTest extends MainVisitorBaseTest {

  test("Test `Length('Spark' + ' SQL ')` function") {
    val code =
      """
        |let foo = Length('Spark' + ' SQL ');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[MInt]
    assert(foo.getValue == 10)
  }

  test("Test `substr('Spark SQL', 5)`") {
    val code =
      """
        |let foo = substr('Spark SQL', 5);
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[MString]
    assert(foo.getValue == "k SQL")
  }

  test("Test `substr('Spark SQL', -3)`") {
    val code =
      """
        |let foo = substr('Spark SQL', -3);
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[MString]
    assert(foo.getValue == "SQL")
  }

  test("Test `substring('Spark SQL', 5, 1)`") {
    val code =
      """
        |let foo = substr('Spark SQL', 5, 1);
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[MString]
    assert(foo.getValue == "k")
  }

  test("Test `substring('Spark SQL', -10, 5)`") {
    val code =
      """
        |let foo = substr('Spark SQL', -10, 5);
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[MString]
    assert(foo.getValue == "Spar")
  }

  test("Test `substring('Spark SQL', -10, 0)`") {
    val code =
      """
        |let foo = substr('Spark SQL', -10, 0);
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[MString]
    assert(foo.getValue == "")
  }
}
