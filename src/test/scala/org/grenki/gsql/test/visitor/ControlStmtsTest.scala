package org.grenki.gsql.test.visitor

import org.scalatest.funsuite.AnyFunSuite
import org.grenki.gsql.{sql, token}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.grenki.gsql.context.Context
import org.grenki.gsql.test.stub.StubEngine
import org.grenki.gsql.engine.Engine
import org.grenki.gsql.visitor.MainVisitor
import scala.collection.mutable.{Map => MutMap}

class ControlStmtsTest extends AnyFunSuite {
  
  def getContext(code: String) = {
    val lexer = new token(CharStreams.fromString(code))
    val tokenStream = new CommonTokenStream(new token(CharStreams.fromString(code)))
    tokenStream.getNumberOfOnChannelTokens // magic. if we do not do this tokenstream is empty
    val parser = new sql(new CommonTokenStream(lexer))
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine))
    new MainVisitor(context, tokenStream).visit(parser.program())
    context
  }
  
  test("Test if: then") {
    val code = """
                |if true then
                |  set res = "if";
                |elif false then
                |  set res = "elif";
                |else
                |  set res = "else";
                |end if
                """.stripMargin
    val context = getContext(code)
    assert(context.vars.contains("res"))
    assert(context.vars("res").toString() == "if")
  }

  test("Test if: elif") {
    val code = """
                |if false then
                |  set res = "if";
                |elif true then
                |  set res = "elif";
                |else
                |  set res = "else";
                |end if
                """.stripMargin
    val context = getContext(code)
    assert(context.vars.contains("res"))
    assert(context.vars("res").toString() == "elif")
  }

  test("Test if: else") {
    val code = """
                |if false then
                |  set res = "if";
                |elif false then
                |  set res = "elif";
                |else
                |  set res = "else";
                |end if
                """.stripMargin
    val context = getContext(code)
    assert(context.vars.contains("res"))
    assert(context.vars("res").toString() == "else")
  }

  test("Test while") {
    val code = """
                |set x = 0;
                |set res = "";
                |while $x < 5 do
                |  set res = $res || $x;
                |  set x = $x + 1;
                |end
                """.stripMargin
    val context = getContext(code)
    assert(context.vars.contains("res"))
    assert(context.vars("res").toString() == "01234")
  }

  test("Test for range") {
    val code = """
                |set res = "";
                |for i in 1..20 step 2 loop
                |  set res = $res || $i;
                |end loop
                """.stripMargin
    val context = getContext(code)
    assert(context.vars.contains("res"))
    val res = context.vars("res")
    assert(context.vars("res").toString() == "13579111315171920")
  }
  // TODO this test fails
  ignore("Test for range reverse") {
    val code = """
                |set res = "";
                |for i in REVERSE 1..20 step 2 loop
                |  set res = $res || $i;
                |end loop
                """.stripMargin
    val context = getContext(code)
    assert(context.vars.contains("res"))
    val res = context.vars("res")
    assert(context.vars("res").toString() == "20181614121086421")
  }
}
