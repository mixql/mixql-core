package org.mixql.core.context

import org.mixql.core.context.gtype.Type

object ControlContext extends Enumeration {
  type ControlContext = Value
  type ReturnType = (Type, ControlContext)
  val RETURN, BREAK, CONTINUE, NONE = Value
}
