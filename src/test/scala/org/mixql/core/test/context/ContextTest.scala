package org.mixql.core.test.context

import org.mixql.core.context.gtype._
import org.mixql.core.context.{Context, EngineContext}
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
    override def paramChangedImpl(name: String, ctx: EngineContext): Unit = {}
  }

  test("Test get vars from config") {
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
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
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", new gInt(12))
    assert(context.getVar("a").isInstanceOf[gInt])
    assert(context.getVar("a").asInstanceOf[gInt].getValue == 12)
  }

  test("Test add null var to context") {
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", new Null)
    assert(isNull(context.getVar("a")))
  }

  test("Test get undefined variable") {
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    assert(isNone(context.getVar("a")))
  }

  test("Test change var value in context") {
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", new gInt(12))
    context.setVar("a", new string("value"))
    assert(context.getVar("a").isInstanceOf[string])
    assert(context.getVar("a").asInstanceOf[string].getValue == "value")
    context.setVar("a", new Null)
    assert(isNull(context.getVar("a")))
  }

  test("Test add engine to context") {
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.addEngine(new MyEngine())
    val e = context.getEngine("MyEngine")
    assert(e != None)
    assert(e.get.isInstanceOf[MyEngine])
  }

  test("Test add engine to context with allias") {
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
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
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.addEngine(new MyEngine())
    context.addEngine("MyEngine1", new MyEngine())
    val e = context.getEngine[MyEngine]
    assert(e != None)
    assert(e.get.name == "MyEngine")
  }

  test("Test get undefined engine by class") {
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    val e = context.getEngine[MyEngine]
    assert(e == None)
  }

  test("Test change current engine") {
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
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
    val context = new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("stub.a", new gInt(12))
    context.setVar("a", new gInt(25))
    val res = context.interpolate("select ${$a + $stub.a}")
    assert(res == "select 37")
  }

  test("Test engine.variables.update = all") {
    val context =
      new Context(
        MutMap[String, Engine]("stub1" -> new StubEngine, "stub2" -> new StubEngine, "stub3" -> new StubEngine),
        "stub1"
      )
    context.getEngine("stub1").get.execute("sdfdsfsdf sd fs", new EngineContext(context))
    context.getEngine("stub2").get.execute("sdfdsfsdf sd fsdfg", new EngineContext(context))
    context.setVar("mixql.engine.variables.update", new string("all"))
    context.setVar("a", new gInt(12))
    assert(context.getVar("a").isInstanceOf[gInt])
    assert(context.getVar("a").asInstanceOf[gInt].getValue == 12)
    // paramChanged was triggered as engine was started before
    assert(context.getEngine("stub1").get.asInstanceOf[StubEngine].changedParams("a").isInstanceOf[gInt])
    // paramChanged was triggered as engine was started before
    assert(context.getEngine("stub1").get.asInstanceOf[StubEngine].changedParams("a").asInstanceOf[gInt].getValue == 12)
    // paramChanged was triggered as engine was started before
    assert(context.getEngine("stub2").get.asInstanceOf[StubEngine].changedParams("a").isInstanceOf[gInt])
    // paramChanged was triggered as engine was started before
    assert(
      context.getEngine("stub2").get.asInstanceOf[StubEngine].changedParams("a").asInstanceOf[gInt].getValue == 12
    ) // paramChanged was not triggered as engine did not started
    assertThrows[java.util.NoSuchElementException](
      context.getEngine("stub3").get.asInstanceOf[StubEngine].changedParams("a").isInstanceOf[gInt]
    )
    // paramChanged was not triggered as engine did not started
    assertThrows[java.util.NoSuchElementException](
      context.getEngine("stub3").get.asInstanceOf[StubEngine].changedParams("a").asInstanceOf[gInt].getValue == 12
    )
  }

  test("Test engine.variables.update = current") {
    val context = new Context(MutMap[String, Engine]("stub1" -> new StubEngine, "stub2" -> new StubEngine), "stub1")
    context.setVar("mixql.engine.variables.update", new string("current"))
    context.getEngine("stub1").get.execute("sdfdsfsdf sd fs", new EngineContext(context))
    context.getEngine("stub2").get.execute("sdfdsfsdf sd fsdfg", new EngineContext(context))
    context.setVar("a", new gInt(12))
    assert(context.getVar("a").isInstanceOf[gInt])
    assert(context.getVar("a").asInstanceOf[gInt].getValue == 12)
    // paramChanged was triggered as engine was started before
    assert(context.getEngine("stub1").get.asInstanceOf[StubEngine].changedParams("a").isInstanceOf[gInt])
    // paramChanged was triggered as engine was started before
    assert(context.getEngine("stub1").get.asInstanceOf[StubEngine].changedParams("a").asInstanceOf[gInt].getValue == 12)
    // paramChanged was triggered as engine was started before
    assert(isNull(context.getEngine("stub2").get.asInstanceOf[StubEngine].changedParams.getOrElse("a", new Null)))
  }

  test("Test engine.variables.update = none") {
    val context = new Context(MutMap[String, Engine]("stub1" -> new StubEngine, "stub2" -> new StubEngine), "stub1")
    context.setVar("mixql.engine.variables.update", new string("none"))
    context.getEngine("stub1").get.execute("sdfdsfsdf sd fs", new EngineContext(context))
    context.getEngine("stub2").get.execute("sdfdsfsdf sd fsdfg", new EngineContext(context))
    context.setVar("a", new gInt(12))
    assert(context.getVar("a").isInstanceOf[gInt])
    assert(context.getVar("a").asInstanceOf[gInt].getValue == 12)
    assert(isNull(context.getEngine("stub1").get.asInstanceOf[StubEngine].changedParams.getOrElse("a", new Null)))
    assert(isNull(context.getEngine("stub2").get.asInstanceOf[StubEngine].changedParams.getOrElse("a", new Null)))
  }

  test("Test scope engine.variables.update = all") {
    val context = new Context(MutMap[String, Engine]("stub1" -> new StubEngine, "stub2" -> new StubEngine), "stub1")
    context.setVar("mixql.engine.variables.update", new string("all"))
    context.getEngine("stub1").get.execute("sdfdsfsdf sd fs", new EngineContext(context))
    context.getEngine("stub2").get.execute("sdfdsfsdf sd fsdfg", new EngineContext(context))
    context.setVar("a", new gInt(12))
    context.push_scope()
    context.setVar("a", new gInt(15))
    context.pop_scope()
    assert(context.getVar("a").isInstanceOf[gInt])
    assert(context.getVar("a").asInstanceOf[gInt].getValue == 12)
    // paramChanged was triggered as engine was started before
    assert(context.getEngine("stub1").get.asInstanceOf[StubEngine].changedParams("a").isInstanceOf[gInt])
    // paramChanged was triggered as engine was started before
    assert(context.getEngine("stub1").get.asInstanceOf[StubEngine].changedParams("a").asInstanceOf[gInt].getValue == 12)
    // paramChanged was triggered as engine was started before
    assert(context.getEngine("stub2").get.asInstanceOf[StubEngine].changedParams("a").isInstanceOf[gInt])
    // paramChanged was triggered as engine was started before
    assert(context.getEngine("stub2").get.asInstanceOf[StubEngine].changedParams("a").asInstanceOf[gInt].getValue == 12)
  }

  test("Test change current engine with scope") {
    val e1 = new StubEngine()
    val e2 = new MyEngine()
    val context = new Context(MutMap[String, Engine]("stub" -> e1), "stub")
    context.addEngine(e2)
    context.push_scope()
    context.setCurrentEngine("MyEngine")
    context.execute("select a from b", false)
    assert(context.currentEngine == e2)
    assert(e2.query == "select a from b")
    val engine_name = context.getVar("mixql.execution.engine")
    assert(engine_name.toString == "MyEngine")
    context.pop_scope()
    assert(context.currentEngine == e1)
    val engine_name1 = context.getVar("mixql.execution.engine")
    assert(engine_name1.toString == "stub")
  }
}
