package org.mixql.core.test.visitor

import org.mixql.core.context.gtype._
import org.mixql.core.test.MainVisitorBaseTest
import org.mixql.core.test.tag.Interpolation

@Interpolation
class StringInterpolationTest extends MainVisitorBaseTest {

  test("Test set with semicolon") {
    val code =
      """
        |let foo = 'abc;123';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "abc;123")
  }

  test("Test set with space") {
    val code =
      """
        |let foo = '    abc    ';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "    abc    ")
  }

  test("Test set with double space") {
    val code =
      """
        |let foo = '    abc   abc  ';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "    abc   abc  ")
  }

  test("Test set with string surrounded spaces") {
    val code =
      """
        |let foo = '    123 abc  ';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "    123 abc  ")
  }

  test("Test set with space and new line") {
    val code =
      s"""
         |let foo = '    123 \n   abc  ';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "    123 \n   abc  ")
  }

  test("Test set with space and new lines") {
    val code =
      s"""
         |let foo = '\n    123 \n   abc  ';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "\n    123 \n   abc  ")
  }

  test("Test set with space new lines and tabulation") {
    val code =
      s"""
         |let foo = '\n    123 \n \t   abc  \n\t';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "\n    123 \n \t   abc  \n\t")
  }

  test("Test set with space new lines and tabulation 2") {
    val code =
      s"""
         |let foo = '\t\n    123 \n \t   abc  \n\t';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "\t\n    123 \n \t   abc  \n\t")
  }

  test("Test set with space new lines and tabulation 3") {
    val code =
      s"""
         |let foo = "\t\n    123 \n \t   abc  \n\t";
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "\t\n    123 \n \t   abc  \n\t")
  }

  test("Test set with space 2") {
    val code = "let foo = 'abc cde';".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "abc cde")
  }

  test("Test set with space started with new line") {
    val code = "let foo = '\nabc cde';".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "\nabc cde")
  }

  test("Test set with new lines") {
    val code = "let foo = '\n\n abc\n cde\n\n\n';".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "\n\n abc\n cde\n\n\n")
  }

  test("Test set with new lines and double quotes") {
    val code = "let foo = \"\n\n abc\n cde\n\n\n\";".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "\n\n abc\n cde\n\n\n")
  }

  test("Test set with new lines and slash quotes") {
    val code = "let foo = `\n\n abc\n cde\n\n\n`;".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "\n\n abc\n cde\n\n\n")
  }

  test("Test set with new lines and single quote") {
    val code = "let foo = '\t\n\n abc\n \tcde\n\n\n';".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "\t\n\n abc\n \tcde\n\n\n")
  }

  //  test("Test when single quotes contains escaped single quotes") {
  //    val code =
  //      "set foo = '\\'';".stripMargin
  //    val context = runMainVisitor(code)
  //    val foo = context.getVar("foo")
  //    assert(foo.isInstanceOf[string])
  //    assert(foo.asInstanceOf[string].getValue == "'")
  //  }

  test("Test when single quotes contains double quotes") {
    val code = "let foo = '\"';".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "\"")
  }

  test("Test when double quotes contains two double quotes") {
    val code = "let foo = '\"\"';".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "\"\"")
  }

  test("Test when double quotes contains single quote") {
    val code = "let foo = \"'\";".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "'")
  }

  test("Test when double quotes contains two single quotes") {
    val code = "let foo = \"''\";".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "''")
  }

  test("Test when slashed quotes contains single quote") {
    val code = "let foo = `'`;".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "'")
  }

  test("Test when slashed quotes contains two single quotes") {
    val code = "let foo = `''`;".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "''")
  }

  test("Test set with interpolation") {
    val code =
      """
        |let v='abc';
        |let foo = '$v;123';
                """.stripMargin
    val context = runMainVisitor(code)

    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "abc;123")
  }

  test("Test set with expression interpolation") {
    val code =
      """
        |let v='a';
        |let foo = '${$v+'bc'};123';
                """.stripMargin
    val context = runMainVisitor(code)

    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "abc;123")
  }

  test("Test set with deep expression interpolation") {
    val code =
      """
        |let v='a';
        |let foo = '${'${$v+'b'}'+'c'};123';
                """.stripMargin
    val context = runMainVisitor(code)

    val foo = context.getVar("foo")
    assert(foo.isInstanceOf[string])
    assert(foo.asInstanceOf[string].getValue == "abc;123")
  }

  test("Test escaped symbols") {
    val code =
      """
        |let v1="dfdgg\ndddd";
        |let v2="fgfhfh\t";
        |let v3="fghfhhgfhg\\n";
        |let v5="gfhfhfhghfhh\\t";
        |let v6="gfhfhfhghfhh$t";
        |let v7="gfhfhfhghfhh\$t";
        |let v8="gfhfhfhghfhh\\";
        |
        |
                """.stripMargin
    val context = runMainVisitor(code)

    assert(context.getVar("v1").isInstanceOf[string])
    assert(
      context.getVar("v1").asInstanceOf[string].getValue ==
        """dfdgg
        |dddd
        |""".stripMargin
    )

    assert(context.getVar("v2").isInstanceOf[string])
    assert(context.getVar("v2").asInstanceOf[string].getValue == "fgfhfh      ")

    assert(context.getVar("v3").isInstanceOf[string])
    assert(context.getVar("v3").asInstanceOf[string].getValue == "fghfhhgfhg\\n")

    assert(context.getVar("v5").isInstanceOf[string])
    assert(context.getVar("v5").asInstanceOf[string].getValue == "gfhfhfhghfhh\\t")

    assert(context.getVar("v6").isInstanceOf[string])
    assert(context.getVar("v6").asInstanceOf[string].getValue == "gfhfhfhghfhhnone")

    assert(context.getVar("v7").isInstanceOf[string])
    assert(context.getVar("v7").asInstanceOf[string].getValue == "gfhfhfhghfhh$t")

    assert(context.getVar("v8").isInstanceOf[string])
    assert(context.getVar("v8").asInstanceOf[string].getValue == "gfhfhfhghfhh\\")
  }
}
