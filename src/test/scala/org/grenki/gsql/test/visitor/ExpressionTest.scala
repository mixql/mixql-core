package org.grenki.gsql.test.visitor

import org.scalatest.funsuite.AnyFunSuite
import org.grenki.gsql.{sqlLexer, sqlParser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.grenki.gsql.context.Context
import org.grenki.gsql.test.stub.StubEngine
import org.grenki.gsql.engine.Engine
import org.grenki.gsql.visitor.MainVisitor
import scala.collection.mutable.{Map => MutMap}
import org.grenki.gsql.context.gtype._

class ExpressionTest extends AnyFunSuite {
  def getContext(code: String) = {
    val lexer = new sqlLexer(CharStreams.fromString(code))
    val tokenStream = new CommonTokenStream(new sqlLexer(CharStreams.fromString(code)))
    tokenStream.getNumberOfOnChannelTokens // magic. if we do not do this tokenstream is empty
    val parser = new sqlParser(new CommonTokenStream(lexer))
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine))
    new MainVisitor(context, tokenStream).visit(parser.program())
    context
  }

  test("Test arithmetic expression") {
    val code = """
                |set a = 0.5;
                |set b = 1.5;
                |set res = (($a + $b) * $a) / 2;
                """.stripMargin
    val context = getContext(code)
    assert(context.vars.contains("res"))
    val res = context.vars("res")
    assert(res.isInstanceOf[double])
    assert(context.vars("res").asInstanceOf[double].value == 0.5)
  }

  test("Test bool expression") {
    val code = """
                |set a = 12
                |set res = $a > 11 and $a < 12;
                |set res1 = $a > 11 or $a < 12;
                """.stripMargin
    val context = getContext(code)
    assert(context.vars.contains("res"))
    assert(context.vars.contains("res1"))
    val res = context.vars("res")
    assert(res.isInstanceOf[bool])
    assert(res.asInstanceOf[bool].value != true)

    val res1 = context.vars("res1")
    assert(res1.isInstanceOf[bool])
    assert(res1.asInstanceOf[bool].value == true)
  }

  test("Test string expression") {
    val code = """
                |set res = 'one' || 2 || true;
                """.stripMargin
    val context = getContext(code)
    assert(context.vars.contains("res"))
    val res = context.vars("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].value == "one2true")
  }

  test("Test case then expression") {
    val code = """
                |set res = case when 2 > 1 then true else 'false' end;
                """.stripMargin
    val context = getContext(code)
    assert(context.vars.contains("res"))
    val res = context.vars("res")
    assert(res.isInstanceOf[bool])
    assert(res.asInstanceOf[bool].value == true)
  }

  test("Test case else expression") {
    val code = """
                |set res = case when 2 < 1 then true else 'false' end;
                """.stripMargin
    val context = getContext(code)
    assert(context.vars.contains("res"))
    val res = context.vars("res")
    assert(res.isInstanceOf[string])
    assert(res.asInstanceOf[string].value == "false")
  }
}
