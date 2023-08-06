package org.mixql.core.exception

import org.mixql.core.context.gtype.{map, string, Type}
import java.util.HashMap;

class UserSqlException(e_type: String, message: String) extends map(new HashMap) {
  update(new string("type"), new string(e_type))
  update(new string("message"), new string(message))
}
