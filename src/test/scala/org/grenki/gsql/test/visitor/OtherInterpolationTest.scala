package org.grenki.gsql.test.visitor

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.grenki.gsql.context.Context
import org.grenki.gsql.context.gtype._
import org.grenki.gsql.visitor.MainVisitor
import org.grenki.gsql.{sqlLexer, sqlParser}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.{Map => MutMap}
import org.grenki.gsql.test.stub.StubEngine
import org.grenki.gsql.engine.Engine
import org.grenki.gsql.test.tag.Interpolation

@Interpolation
class OtherInterpolationTest extends AnyFunSuite {
  
  def getContext(code: String, context: Context = new Context()): Context = {
    val lexer = new sqlLexer(CharStreams.fromString(code))
    val tokenStream = new CommonTokenStream(new sqlLexer(CharStreams.fromString(code)))
    tokenStream.getNumberOfOnChannelTokens // magic. if we do not do this tokenstream is empty
    val parser = new sqlParser(new CommonTokenStream(lexer))
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine))
    new MainVisitor(context, tokenStream).visit(parser.program())
    context
  }

  test("Test other statement string interpolation") {
    val code = """
                |set gg = 10;
                |select '${$gg * 5}' from wp;
                """.stripMargin
    val context = getContext(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(query.dequeue() == "select '50' from wp")
  }
}
