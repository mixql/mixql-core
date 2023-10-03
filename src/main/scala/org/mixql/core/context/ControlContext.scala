package org.mixql.core.context

import org.mixql.core.context.mtype._

object ControlContext extends Enumeration {
  type ControlContext = Value
  type ReturnType = (MType, ControlContext)
  val RETURN, BREAK, CONTINUE, NONE = Value
}
