package org.grenki.gsql.test.stub

import scala.collection.mutable.Queue
import org.grenki.gsql.engine.Engine

class StubEngine extends Engine {
    val queue = new Queue[String]()

    override def execute(stmt: String): Unit = {
        queue += stmt
    }
}
