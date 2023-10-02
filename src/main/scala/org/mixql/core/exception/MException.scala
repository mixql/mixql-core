package org.mixql.core.exception

import org.mixql.core.context.mtype._
import java.util.HashMap;

class MException(e_type: String, message: String) extends MMap(new HashMap) {
  update(new MString("type"), new MString(e_type))
  update(new MString("message"), new MString(message))
}
