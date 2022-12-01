package org.mixql.core.test.function

import org.mixql.core.context.gtype._
import org.mixql.core.test.MainVisitorBaseTest

class ConcatFunctionsTest extends MainVisitorBaseTest {
  test("Test `concat('Spark', 'SQL') == 'SparkSQL'`") {
    val code =
      """
        |let foo = concat('Spark', 'SQL');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "SparkSQL")
  }

  test("Test `concat_ws('|','A','B','C') == 'A|B|C'`") {
    val code =
      """
        |let foo = concat_ws('|','A','B','C');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "A|B|C")
  }

  test("Test `concat_ws('|','A','B','C',NULL,'D') == 'A|B|C|D'`") {
    val code =
      """
        |let foo = concat_ws('|','A','B','C',NULL,'D');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "A|B|C|D")
  }
}
