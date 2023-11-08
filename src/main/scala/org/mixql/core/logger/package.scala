package org.mixql.core

import org.apache.logging.log4j.LogManager

package object logger {
  private val log = LogManager.getRootLogger

  def logInfo(msg: String) = { log.info("[mixql-core] " + msg) }

  def logDebug(msg: String) = { log.debug("[mixql-core] " + msg) }

  def logWarn(msg: String) = { log.warn("[mixql-core] " + msg) }

  def logError(msg: String) = { log.error("[mixql-core] " + msg) }
}
