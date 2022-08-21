package org.grenki.gsql.test

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.grenki.gsql.context.Context
import org.grenki.gsql.context.gtype.{Type, string}
import org.grenki.gsql.visitor.MainVisitor
import org.grenki.gsql.{sqlLexer, sqlParser}
import org.scalatest.funsuite.AnyFunSuite

class StringInterpolationTest extends AnyFunSuite {

  test("Test set with semicolon") {
    val code =
      """
        |set foo = 'abc;123';
                """.stripMargin
    val context = runMainVisitor(code)
    val foo = context.getVar("foo").asInstanceOf[string]
    assert(foo.value == "abc;123")
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

  def runMainVisitor(code: String, context: Context[Type] = new Context[Type]()): Context[Type] = {
    val lexer = new sqlLexer(CharStreams.fromString(code))
    val tokenStream = new CommonTokenStream(new sqlLexer(CharStreams.fromString(code)))
    tokenStream.getNumberOfOnChannelTokens // magic. if we do not do this tokenstream is empty
    val parser = new sqlParser(new CommonTokenStream(lexer))
    new MainVisitor(context, tokenStream).visit(parser.program())
    context
  }
}