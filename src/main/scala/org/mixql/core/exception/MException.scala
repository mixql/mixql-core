package org.mixql.core.exception

import org.mixql.core.context.mtype._
import java.util.HashMap;

class MException(e_type: String, message: String, val e: Throwable = null) extends MMap(new HashMap) {

  def this(e: Throwable) = {
    this(e.getClass.getSimpleName, e.getMessage, e)
  }

  update(new MString("type"), new MString(e_type))
  update(new MString("message"), new MString(message))

  override def getCause(): Throwable = e
}
