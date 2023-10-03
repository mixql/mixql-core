package org.mixql.core.test.context

import org.mixql.core.context.mtype._
import org.mixql.core.context.{EngineContext, Context}

import org.mixql.core.engine.Engine
import org.mixql.core.test.engines.StubEngine
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.{Map => MutMap}

class ContextTest extends AnyFunSuite {

  class MyEngine extends Engine {
    var query: String = ""
    override def name: String = "MyEngine"

    override def executeImpl(stmt: String, ctx: EngineContext): MType = {
      query = stmt
      MNull.get()
    }

    override def executeFuncImpl(name: String, ctx: EngineContext, kwargs: Map[String, Object], params: MType*): MType =
      ???
  }

  test("Test get vars from config") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    assert(isNull(context.getVar("nullVariable")))
    assert(context.getVar("boolVariable").isInstanceOf[MBool])
    assert(context.getVar("boolVariable").asInstanceOf[MBool].getValue == true)
    assert(context.getVar("intVariable").isInstanceOf[MInt])
    assert(context.getVar("intVariable").asInstanceOf[MInt].getValue == 42)
    assert(context.getVar("doubleVariable").isInstanceOf[MDouble])
    assert(context.getVar("doubleVariable").asInstanceOf[MDouble].getValue == 42.42)
    assert(context.getVar("strVariable").isInstanceOf[MString])
    assert(context.getVar("strVariable").asInstanceOf[MString].getValue == "str")
    assert(context.getVar("variable.with.points").isInstanceOf[MString])
    assert(
      context.getVar("variable.with.points").asInstanceOf[MString].getValue ==
        "var with pt"
    )
    assert(context.getVar("variable.with.points2").isInstanceOf[MInt])
    assert(
      context.getVar("variable.with.points2").asInstanceOf[MInt].getValue ==
        1725
    )
    assert(context.getVar("listVariable").isInstanceOf[MArray])
    val arr = context.getVar("listVariable").asInstanceOf[MArray]
    assert(arr.size == new MInt(4))
    assert(arr(new MInt(0)).isInstanceOf[MBool])
    assert(arr(new MInt(1)).isInstanceOf[MInt])
    assert(arr(new MInt(2)).isInstanceOf[MDouble])
    assert(arr(new MInt(3)).isInstanceOf[MString])
  }

  test("Test add var value to context") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", new MInt(12))
    assert(context.getVar("a").isInstanceOf[MInt])
    assert(context.getVar("a").asInstanceOf[MInt].getValue == 12)
  }

  test("Test add null var to context") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", MNull.get())
    assert(isNull(context.getVar("a")))
  }

  test("Test get undefined variable") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    assert(isNone(context.getVar("a")))
  }

  test("Test change var value in context") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", new MInt(12))
    context.setVar("a", new MString("value"))
    assert(context.getVar("a").isInstanceOf[MString])
    assert(context.getVar("a").asInstanceOf[MString].getValue == "value")
    context.setVar("a", MNull.get())
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
    context.setVar("stub.a", new MInt(12))
    context.setVar("a", new MInt(25))
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

  test("Test fork context") {
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", new MInt(25))
    val fork = context.fork()
    assert(fork.getVar("a").asInstanceOf[MInt].getValue == 25)
    context.pushScope()

    context.setVar("a", new MInt(15))
    assert(fork.getVar("a").asInstanceOf[MInt].getValue == 25)
    assert(context.getVar("a").asInstanceOf[MInt].getValue == 15)

    fork.setVar("a", new MInt(35))
    assert(fork.getVar("a").asInstanceOf[MInt].getValue == 35)
    assert(context.getVar("a").asInstanceOf[MInt].getValue == 15)

    context.setVar("a", new MInt(45))
    assert(fork.getVar("a").asInstanceOf[MInt].getValue == 35)
    assert(context.getVar("a").asInstanceOf[MInt].getValue == 45)

    fork.setVar("a", new MInt(55))
    assert(fork.getVar("a").asInstanceOf[MInt].getValue == 55)
    assert(context.getVar("a").asInstanceOf[MInt].getValue == 45)
  }
}
