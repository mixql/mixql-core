package org.grenki.gsql.test.function

import org.grenki.gsql.context.gtype.{int, string}
import org.grenki.gsql.test.MainVisitorBaseTest

class Base64FunctionsTest extends MainVisitorBaseTest {
  test("Test `base64('Spark SQL') == 'U3BhcmsgU1FM'`") {
    val code =
      """
        |let foo = base64('Spark SQL');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "U3BhcmsgU1FM")
  }
}
