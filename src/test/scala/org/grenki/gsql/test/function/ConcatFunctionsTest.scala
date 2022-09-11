package org.grenki.gsql.test.function

import org.grenki.gsql.context.gtype.string
import org.grenki.gsql.test.MainVisitorBaseTest

class ConcatFunctionsTest extends MainVisitorBaseTest {
  test("Test `concat('Spark', 'SQL') == 'SparkSQL'`") {
    val code =
      """
        |set foo = concat('Spark', 'SQL');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "SparkSQL")
  }

  test("Test `concat_ws('|','A','B','C') == 'A|B|C'`") {
    val code =
      """
        |set foo = concat_ws('|','A','B','C');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "A|B|C")
  }

  test("Test `concat_ws('|','A','B','C',NULL,'D') == 'A|B|C|D'`") {
    val code =
      """
        |set foo = concat_ws('|','A','B','C',NULL,'D');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "A|B|C|D")
  }
}