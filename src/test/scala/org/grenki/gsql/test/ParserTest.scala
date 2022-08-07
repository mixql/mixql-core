package org.grenki.gsql.test

import org.scalatest.funsuite.AnyFunSuite
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.grenki.gsql.{sqlLexer, sqlParser}
import collection.JavaConverters._

class ParserTest extends AnyFunSuite {

  test("Test `some code to run;` is any_coma rule") {
    val code = """
                |some code to run;
                """.stripMargin
    val lexer = new sqlLexer(CharStreams.fromString(code))
    val parser = new sqlParser(new CommonTokenStream(lexer))
    val stmts = parser.programm().statment()
    assert(stmts.size == 1)
    val any_coma = stmts.get(0).any_comma()
    assert(any_coma != null)
    val any_coma_text = any_coma.getText 
    assert(any_coma_text == "somecodetorun;")
  }

}