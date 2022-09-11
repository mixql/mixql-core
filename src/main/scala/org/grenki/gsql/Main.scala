package org.grenki.gsql

import scala.collection.mutable.{Map => MutMap}
import org.grenki.gsql.context.Context
import org.grenki.gsql.context.gtype.Type
import org.grenki.gsql.visitor.MainVisitor

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.grenki.gsql.engine.Engine

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
        |select '${$gg * 5}' from wp;
        |print("\$res");
        |print("$res");
        |print("${$res}");
        |print("${'${'$res'+'a'}'}\"");
        |set t="${'1'+$res}";
        |set t="123;
        |   \n 456";
        |print($t);
        |set t="${'1'+ '${$res}'}";
        |print($t);
        |some end;""".stripMargin
    execute(code)
  }

  def execute(code: String): Unit = {
    val lexer = new token(CharStreams.fromString(code))
    val tokenStream = new CommonTokenStream(new token(CharStreams.fromString(code)))
    tokenStream.getNumberOfOnChannelTokens // magic. if we do not do this tokenstream is empty
    val parser = new sql(new CommonTokenStream(lexer))

    val context = new Context(MutMap[String, Engine]("stub" -> new Engine))

    new MainVisitor(context, tokenStream).visit(parser.program())
    println(context.vars)
  }
}