package org.grenki.gsql.engine

import org.grenki.gsql.context.gtype._

class Engine {
  def execute(stmt: String): Type = {
    println("execute: " + stmt)
    Null
  }
}
