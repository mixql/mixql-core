package org.mixql.core.test.function

import org.mixql.core.context.gtype._
import org.mixql.core.test.MainVisitorBaseTest

class Base64FunctionsTest extends MainVisitorBaseTest {
  test("Test `base64('Spark SQL') == 'U3BhcmsgU1FM'`") {
    val code =
      """
        |let foo = base64('Spark SQL');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.getValue == "U3BhcmsgU1FM")
  }
}
