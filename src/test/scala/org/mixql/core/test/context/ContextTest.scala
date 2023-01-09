package org.mixql.core.test.context

import org.mixql.core.context.gtype._
import org.mixql.core.context.Context
import org.mixql.core.engine.Engine
import org.mixql.core.test.stub.StubEngine
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.{Map => MutMap}

class ContextTest extends AnyFunSuite {
  def isNull(v: Type): Boolean = {
    v match {
      case Null => true
      case _    => false
    }
  }

  class MyEngine extends Engine {
    var query: String = ""
    override def name: String = "MyEngine"
    override def execute(stmt: String): Type = {
      query = stmt
      Null
    }
    override def executeFunc(name: String, params: Type*): Type = ??? 
    override def getParam(name: String): Type = Null
    override def setParam(name: String, value: Type): Unit = {}
  }

  test("Test add var value to context") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", int(12))
    assert(context.getVar("a").isInstanceOf[int])
    assert(context.getVar("a").asInstanceOf[int].value == 12)
  }

  test("Test add null var to context") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", Null)
    assert(isNull(context.getVar("a")))
  }

  test("Test get undefined variable") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    assert(isNull(context.getVar("a")))
  }

  test("Test change var value in context") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", int(12))
    context.setVar("a", string("value"))
    assert(context.getVar("a").isInstanceOf[string])
    assert(context.getVar("a").asInstanceOf[string].value == "value")
    context.setVar("a", Null)
    assert(isNull(context.getVar("a")))
  }

  test("Test add engine to context") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.addEngine(new MyEngine())
    val e = context.getEngine("MyEngine")
    assert(e != None)
    assert(e.get.isInstanceOf[MyEngine])
  }

  test("Test add engine to context with allias") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
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
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.addEngine(new MyEngine())
    context.addEngine("MyEngine1", new MyEngine())
    val e = context.getEngine[MyEngine]
    assert(e != None)
    assert(e.get.name == "MyEngine")
  }

  test("Test get undefined engine by class") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    val e = context.getEngine[MyEngine]
    assert(e == None)
  }

  test("Test change current engine") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    val e = new MyEngine()
    context.addEngine(e)
    context.setCurrentEngine("MyEngine")
    context.execute("select a from b")
    assert(context.currentEngine == e)
    assert(e.query == "select a from b")
    val engine_name = context.getVar("grenki.execution.engine")
    assert(engine_name.toString == "MyEngine")
  }

  test("Test interpolator") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("stub.a", int(12))
    context.setVar("a", int(25))
    val res = context.interpolate("select ${$a + $stub.a}")
    assert(res == "select 37")
  }
}
