package org.mixql.core.exception

class UserSqlException(e_type: String, m: String) extends Exception {
  val exc_type = e_type
  val detailMessage = m
}
