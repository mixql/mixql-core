package org.grenki.gsql.test.visitor

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.grenki.gsql.context.Context
import org.grenki.gsql.context.gtype._
import org.grenki.gsql.visitor.MainVisitor
import org.grenki.gsql.{sql, token}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.{Map => MutMap}
import org.grenki.gsql.test.stub.StubEngine
import org.grenki.gsql.engine.Engine
import org.grenki.gsql.test.tag.Interpolation

@Interpolation
class OtherInterpolationTest extends AnyFunSuite {
  
  def getContext(code: String, context: Context = new Context()): Context = {
    val lexer = new token(CharStreams.fromString(code))
    val tokenStream = new CommonTokenStream(new token(CharStreams.fromString(code)))
    tokenStream.getNumberOfOnChannelTokens // magic. if we do not do this tokenstream is empty
    val parser = new sql(new CommonTokenStream(lexer))
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine))
    new MainVisitor(context, tokenStream).visit(parser.program())
    context
  }

  test("Test any statement variable interpolation") {
    val code = """
                |set a = 10;
                |select $a from table where column > 10;
                """.stripMargin
    val context = getContext(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(query.dequeue() == "select 10 from table where column > 10")
  }

  test("Test other statement expression interpolation") {
    val code = """
                |set a = 10;
                |select ${$a + 3} from table where column > 10;
                """.stripMargin
    val context = getContext(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(query.dequeue() == "select 13 from table where column > 10")
  }

  test("Test other statement string interpolation") {
    val code = """
                |set a = 10;
                |set b = 'some str';
                |select '${$a || ' df;df $b'}' from table where column > 10;
                """.stripMargin
    val context = getContext(code)
    val query = context.currentEngine.asInstanceOf[StubEngine].queue
    assert(query.dequeue() == "select '10 df;df some str' from table where column > 10")
  }
}
