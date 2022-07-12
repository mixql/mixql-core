package org.grenki.gsql

import context.Context
import context.`type`.Type
import visitor.Visitor

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

object Main {
  def main(args: Array[String]): Unit = {
    val code =
      """some code;
        |set gg = 12.4 - 11.2;
        |set wp.x = $gg > 11;
        |set res = 'one' + 'two';
        |set check_case = case when 1 > 2 then 12 when 1 < 2 then 13 else '12g' end;
        |if 12 < 11 then
        |  print(true);
        |else
        |  print(false);
        |end if
        |set x = 0;
        |while $x < 5 do
        |  print($x);
        |  set x = $x + 1;
        |end
        |for i in 1..20 step 2 loop
        |  print($i);
        |end loop
        |select '${$gg + 2}' from wp;
        |some end;""".stripMargin
    execute(code)
  }

  def execute(code: String): Unit = {
    val lexer = new sqlLexer(CharStreams.fromString(code))
    val tokenStream = new CommonTokenStream(new sqlLexer(CharStreams.fromString(code)))
    tokenStream.getNumberOfOnChannelTokens // magic. if we do not do this tokenstream is empty
    val parser = new sqlParser(new CommonTokenStream(lexer))

    val context = new Context[Type]()

    new Visitor(context, tokenStream).visit(parser.programm())
    println(context.vars)
  }
}