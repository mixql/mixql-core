package org.mixql.core

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.mixql.core.context.gtype._
import org.mixql.core
import org.mixql.core.context.Context
import org.mixql.core.engine.Engine
import org.mixql.core.logger.{logDebug, logInfo}

import scala.collection.mutable.{Map => MutMap}

object Main {
  class DemoEngine extends Engine {
    override def name: String = "demo"

    override def execute(stmt: String): Type = {
      logDebug("[DemoEngine] execute: " + stmt)
      new Null()
    }

    override def executeFunc(name: String, params: Type*) = new Null()

    override def setParam(name: String, value: Type): Unit = {}

    override def getParam(name: String): Type = new Null()

    override def isParam(name: String): Boolean = true
  }

  val code =
    """some code;;
      |let gg = 12.4 - 11.2;
      |let wp.x = $gg > 11;
      |let res = 'one' + 'two';
      |let check_case = case when 1 > 2 then 12 when 1 < 2 then 13 else '12g' end;
      |if 12 < 11 then
      |  print(true);
      |else
      |  print(false);
      |end if
      |let x = 0;
      |while $x < 5 do
      |  print($x);
      |  let x = $x + 1;
      |end while
      |for i in 1..20 step 2 loop
      |  print($i);
      |end loop
      |select '${$gg + 2}' from wp;
      |select '${$gg * 5}' from wp;
      |print("\$res");
      |print("$res");
      |print("${$res}");
      |print("${'${'$res'+'a'}'}\"");
      |let t="${'1'+$res}";
      |let t="123;
      |   \n 456";
      |print($t);
      |let t="${'1'+ '${$res}'}";
      |print($t);
      |some end;
      |print(current_timestamp);
      |let arr = [3, 'gg'];
      |let arr[0] = 4;
      |print($arr[0]);
      |let mapa = {1: 1, "1": 2};
      |print($mapa[1]);
      |print($mapa["1"]);""".stripMargin

  def main(args: Array[String]): Unit = {
    val context =
      new Context(MutMap[String, Engine]("demo" -> new DemoEngine), "demo")
    val res = core.run(code, context)
    logDebug(context.getScope().head.toString())
    logDebug(res.toString)
  }
}
