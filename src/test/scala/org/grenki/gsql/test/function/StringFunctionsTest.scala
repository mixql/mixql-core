package org.grenki.gsql.test.function

import org.grenki.gsql.context.gtype.{int, string}
import org.grenki.gsql.test.MainVisitorBaseTest

class StringFunctionsTest extends MainVisitorBaseTest {
  test("Test `Length('Spark' + ' SQL ')` function") {
    val code =
      """
        |set foo = Length('Spark' + ' SQL ');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[int]
    assert(foo.value == 10)
  }

  test("Test `substr('Spark SQL', 5)`") {
    val code =
      """
        |set foo = substr('Spark SQL', 5);
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "k SQL")
  }

  test("Test `substr('Spark SQL', -3)`") {
    val code =
      """
        |set foo = substr('Spark SQL', -3);
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "SQL")
  }

  test("Test `substring('Spark SQL', 5, 1)`") {
    val code =
      """
        |set foo = substr('Spark SQL', 5, 1);
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "k")
  }

  test("Test `substring('Spark SQL', -10, 5)`") {
    val code =
      """
        |set foo = substr('Spark SQL', -10, 5);
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "Spar")
  }

  test("Test `substring('Spark SQL', -10, 0)`") {
    val code =
      """
        |set foo = substr('Spark SQL', -10, 0);
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "")
  }
}