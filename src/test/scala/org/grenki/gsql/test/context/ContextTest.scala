package org.grenki.gsql.test.context

import org.grenki.gsql.context.gtype._
import org.grenki.gsql.context.Context
import org.grenki.gsql.engine.Engine
import org.grenki.gsql.test.stub.StubEngine
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.{Map => MutMap}

class ControlStmtsTest extends AnyFunSuite {
  def isNull(v: Type): Boolean = {
    v match {
      case Null => true
      case _    => false
    }
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
    class MyEngine extends Engine {
      override def name: String = "MyEngine"
    }
    context.addEngine(new MyEngine())
    val e = context.getEngine("MyEngine")
    assert(e != None)
    assert(e.get.isInstanceOf[MyEngine])
  }

  test("Test add engine to context with allias") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    class MyEngine extends Engine {
      override def name: String = "MyEngine"
    }
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
    class MyEngine extends Engine {
      override def name: String = "MyTestEngine"
    }
    context.addEngine(new MyEngine())
    context.addEngine("MyEngine1", new MyEngine())
    val e = context.getEngine[MyEngine]
    assert(e != None)
    assert(e.get.name == "MyTestEngine")
  }

  test("Test get undefined engine by class") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    class MyEngine extends Engine {
      override def name: String = "MyTestEngine"
    }
    val e = context.getEngine[MyEngine]
    assert(e == None)
  }

  test("Test change current engine") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    class MyEngine extends Engine {
      var query: String = ""
      override def name: String = "MyTestEngine"
      override def execute(stmt: String): Type = {
        query = stmt
        Null
      }
    }
    val e = new MyEngine()
    context.addEngine(e)
    context.setCurrentEngine("MyTestEngine")
    context.execute("select a from b")
    assert(context.currentEngine == e)
    assert(e.query == "select a from b")
    val engine_name = context.getVar("grenki.execution.engine")
    assert(engine_name.toString == "MyTestEngine")
  }

  test("Test change current engine using grenki.execution.engine param") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    class MyEngine extends Engine {
      var query: String = ""
      override def name: String = "MyTestEngine"
      override def execute(stmt: String): Type = {
        query = stmt
        Null
      }
    }
    val e = new MyEngine()
    context.addEngine(e)
    context.setVar("grenki.execution.engine", string("MyTestEngine"))
    context.execute("select a from b")
    assert(context.currentEngine == e)
    assert(e.query == "select a from b")
    val engine_name = context.getVar("grenki.execution.engine")
    assert(engine_name.toString == "MyTestEngine")
  }

  test("Test change engine param") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("stub.a", int(12))
    val p = context.getVar("stub.a")
    assert(p.isInstanceOf[int])
    assert(p.asInstanceOf[int].value == 12)
    assert(context.currentEngine.getParam("stub.a").isInstanceOf[int])
    assert(
      context.currentEngine.getParam("stub.a").asInstanceOf[int].value == 12
    )
  }
  // TODO params from other engines
  test("Test interpolator") {
    val context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("stub.a", int(12))
    context.setVar("a", int(25))
    val res = context.interpolate("select ${$a + $stub.a}")
    assert(res == "select 37")
  }
}
