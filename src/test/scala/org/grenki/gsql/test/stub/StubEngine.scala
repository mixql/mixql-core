package org.grenki.gsql.test.stub

import scala.collection.mutable.Queue
import org.grenki.gsql.engine.Engine
import org.grenki.gsql.context.gtype._

class StubEngine extends Engine {
  val queue = new Queue[String]()

  override def name: String = "stub"
  override def execute(stmt: String): Type = {
    queue += stmt
    Null
  }
}
