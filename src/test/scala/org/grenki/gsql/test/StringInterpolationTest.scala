package org.grenki.gsql.test

import org.grenki.gsql.context.gtype.string

class StringInterpolationTest extends MainVisitorBaseTest {

  test("Test set with semicolon") {
    val code =
      """
        |set foo = 'abc;123';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "abc;123")
  }

  test("Test set with space") {
    val code =
      """
        |set foo = '    abc    ';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "    abc    ")
  }

  test("Test set with double space") {
    val code =
      """
        |set foo = '    abc   abc  ';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "    abc   abc  ")
  }

 test("Test set with string surrounded spaces") {
    val code =
      """
        |set foo = '    123 abc  ';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "    123 abc  ")
  }

  test("Test set with space and new line") {
    val code =
      s"""
        |set foo = '    123 \n   abc  ';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "    123 \n   abc  ")
  }

  test("Test set with space and new lines") {
    val code =
      s"""
        |set foo = '\n    123 \n   abc  ';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "\n    123 \n   abc  ")
  }

  test("Test set with space new lines and tabulation") {
    val code =
      s"""
        |set foo = '\n    123 \n \t   abc  \n\t';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "\n    123 \n \t   abc  \n\t")
  }

  test("Test set with space new lines and tabulation 2") {
    val code =
      s"""
        |set foo = '\t\n    123 \n \t   abc  \n\t';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "\t\n    123 \n \t   abc  \n\t")
  }

  test("Test set with space new lines and tabulation 3") {
    val code =
      s"""
         |set foo = "\t\n    123 \n \t   abc  \n\t";
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "\t\n    123 \n \t   abc  \n\t")
  }

  test("Test set with space 2") {
    val code =
      "set foo = 'abc cde';".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value =="abc cde")
  }

  test("Test set with space started with new line") {
    val code =
      "set foo = '\nabc cde';".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value =="\nabc cde")
  }

  test("Test set with new lines") {
    val code =
      "set foo = '\n\n abc\n cde\n\n\n';".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value =="\n\n abc\n cde\n\n\n")
  }

  test("Test set with new lines and double quotes") {
    val code =
      "set foo = \"\n\n abc\n cde\n\n\n\";".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value =="\n\n abc\n cde\n\n\n")
  }

  test("Test set with new lines and slash quotes") {
    val code =
      "set foo = `\n\n abc\n cde\n\n\n`;".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value =="\n\n abc\n cde\n\n\n")
  }

  test("Test set with new lines and single quote") {
    val code =
      "set foo = '\t\n\n abc\n \tcde\n\n\n';".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value =="\t\n\n abc\n \tcde\n\n\n")
  }

//  test("Test when single quotes contains escaped single quotes") {
//    val code =
//      "set foo = '\\'';".stripMargin
//    val context = runMainVisitor(code)
//    val foo = context.getVar("foo").asInstanceOf[string]
//
//    assert(foo.value == "'")
//  }


  test("Test when single quotes contains double quotes") {
    val code =
      "set foo = '\"';".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value =="\"")
  }

  test("Test when double quotes contains two double quotes") {
    val code =
      "set foo = '\"\"';".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value =="\"\"")
  }

  test("Test when double quotes contains single quote") {
    val code =
      "set foo = \"'\";".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value =="'")
  }

  test("Test when double quotes contains two single quotes") {
    val code =
      "set foo = \"''\";".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value =="''")
  }

  test("Test when slashed quotes contains single quote") {
    val code =
      "set foo = `'`;".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value =="'")
  }

  test("Test when slashed quotes contains two single quotes") {
    val code =
      "set foo = `''`;".stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value =="''")
  }

  test("Test set with interpolation") {
    val code =
      """
        |set v='abc';
        |set foo = '$v;123';
                """.stripMargin
    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "abc;123")
  }

  test("Test set with expression interpolation") {
    val code =
      """
        |set v='a';
        |set foo = '${$v+'bc'};123';
                """.stripMargin
    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "abc;123")
  }

  test("Test set with deep expression interpolation") {
    val code =
      """
        |set v='a';
        |set foo = '${'${$v+'b'}'+'c'};123';
                """.stripMargin
    val context = runMainVisitor(code)

    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "abc;123")
  }
}