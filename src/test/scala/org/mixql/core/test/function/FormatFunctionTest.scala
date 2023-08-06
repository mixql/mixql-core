package org.mixql.core.test.function

import org.mixql.core.context.gtype._
import org.mixql.core.test.MainVisitorBaseTest

class FormatFunctionTest extends MainVisitorBaseTest {

  test("Test `format_number(12332.123456, 4) == '12,332.1235'`") {
    val code =
      """
        |let foo = format_number(12332.123456, 4);
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.getValue == "12,332.1235")
  }

  ignore("Test `format_number(12332.123456, '#.###') == '12332.123'`") {
    val code =
      """
        |let foo = format_number(12332.123456, '#.###');
        |""".stripMargin

    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.getValue == "12332.123")
  }
}
