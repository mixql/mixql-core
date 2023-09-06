package org.mixql.core.test.context

import org.mixql.core.context.gtype._
import org.mixql.core.context.{EngineContext, Context}

import org.mixql.core.engine.Engine
import org.mixql.core.test.engines.StubEngine
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.{Map => MutMap}

class ContextTest extends AnyFunSuite {

  class MyEngine extends Engine {
    var query: String = ""
    override def name: String = "MyEngine"

    override def executeImpl(stmt: String, ctx: EngineContext): Type = {
      query = stmt
      new Null()
    }

    override def executeFuncImpl(name: String, ctx: EngineContext, kwargs: Map[String, Object], params: Type*): Type =
      ???
  }

  test("Test get vars from config") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    assert(isNull(context.getVar("nullVariable")))
    assert(context.getVar("boolVariable").isInstanceOf[bool])
    assert(context.getVar("boolVariable").asInstanceOf[bool].getValue == true)
    assert(context.getVar("intVariable").isInstanceOf[gInt])
    assert(context.getVar("intVariable").asInstanceOf[gInt].getValue == 42)
    assert(context.getVar("doubleVariable").isInstanceOf[gDouble])
    assert(context.getVar("doubleVariable").asInstanceOf[gDouble].getValue == 42.42)
    assert(context.getVar("strVariable").isInstanceOf[string])
    assert(context.getVar("strVariable").asInstanceOf[string].getValue == "str")
    assert(context.getVar("variable.with.points").isInstanceOf[string])
    assert(
      context.getVar("variable.with.points").asInstanceOf[string].getValue ==
        "var with pt"
    )
    assert(context.getVar("variable.with.points2").isInstanceOf[gInt])
    assert(
      context.getVar("variable.with.points2").asInstanceOf[gInt].getValue ==
        1725
    )
    assert(context.getVar("listVariable").isInstanceOf[array])
    val arr = context.getVar("listVariable").asInstanceOf[array]
    assert(arr.size == new gInt(4))
    assert(arr(new gInt(0)).isInstanceOf[bool])
    assert(arr(new gInt(1)).isInstanceOf[gInt])
    assert(arr(new gInt(2)).isInstanceOf[gDouble])
    assert(arr(new gInt(3)).isInstanceOf[string])
  }

  test("Test add var value to context") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", new gInt(12))
    assert(context.getVar("a").isInstanceOf[gInt])
    assert(context.getVar("a").asInstanceOf[gInt].getValue == 12)
  }

  test("Test add null var to context") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", new Null)
    assert(isNull(context.getVar("a")))
  }

  test("Test get undefined variable") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    assert(isNone(context.getVar("a")))
  }

  test("Test change var value in context") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", new gInt(12))
    context.setVar("a", new string("value"))
    assert(context.getVar("a").isInstanceOf[string])
    assert(context.getVar("a").asInstanceOf[string].getValue == "value")
    context.setVar("a", new Null)
    assert(isNull(context.getVar("a")))
  }

  test("Test add engine to context") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.addEngine(new MyEngine())
    val e = context.getEngine("MyEngine")
    assert(e != None)
    assert(e.get.isInstanceOf[MyEngine])
  }

  test("Test add engine to context with allias") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.addEngine("MyEngine1", new MyEngine())
    context.addEngine("MyEngine2", new MyEngine())
    val e = context.getEngine("MyEngine")
    assert(e == None)
    val e1 = context.getEngine("MyEngine1")
    assert(e1 != None)
    assert(e1.get.isInstanceOf[MyEngine])
    val e2 = context.getEngine("MyEngine2")
    assert(e2 != None)
    assert(e2.get.isInstanceOf[MyEngine])
  }

  test("Test get engine by class") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.addEngine(new MyEngine())
    context.addEngine("MyEngine1", new MyEngine())
    val e = context.getEngine[MyEngine]
    assert(e != None)
    assert(e.get.name == "MyEngine")
  }

  test("Test get undefined engine by class") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    val e = context.getEngine[MyEngine]
    assert(e == None)
  }

  test("Test change current engine") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    val e = new MyEngine()
    context.addEngine(e)
    context.setCurrentEngine("MyEngine")
    context.execute("select a from b", false)
    assert(context.currentEngine == e)
    assert(e.query == "select a from b")
    val engine_name = context.getVar("mixql.execution.engine")
    assert(engine_name.toString == "MyEngine")
  }

  test("Test interpolator") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("stub.a", new gInt(12))
    context.setVar("a", new gInt(25))
    val res = context.interpolate("select ${$a + $stub.a}")
    assert(res == "select 37")
  }

  test("Test change current engine with scope") {
    val e1 = new StubEngine()
    val e2 = new MyEngine()
    val context = Context(MutMap[String, Engine]("stub" -> e1), "stub")
    context.addEngine(e2)
    context.pushScope()
    context.setCurrentEngine("MyEngine")
    context.execute("select a from b", false)
    assert(context.currentEngine == e2)
    assert(e2.query == "select a from b")
    val engine_name = context.getVar("mixql.execution.engine")
    assert(engine_name.toString == "MyEngine")
    context.popScope()
    assert(context.currentEngine == e1)
    val engine_name1 = context.getVar("mixql.execution.engine")
    assert(engine_name1.toString == "stub")
  }
}
