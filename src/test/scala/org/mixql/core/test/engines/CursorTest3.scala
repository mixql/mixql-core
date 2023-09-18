package org.mixql.core.test.engines

import org.mixql.core.context.gtype.{Type, bool, cursor, gInt, none}
import org.mixql.core.logger.logInfo

import scala.util.Random

class CursorTest3() extends cursor {

  val countToFetch = 10;
  var currentCount = 0;

  var stream: Random = null

  override def close(): bool = {
    logInfo("close was triggered in CursorTest3")
    new bool(true)
  }

  override def open(): bool = {
    logInfo("open was triggered in CursorTest3")
    if (stream == null)
      stream = new scala.util.Random

    new bool(true)
  }

  override def fetch(): Type = {
    logInfo("fetch was triggered in CursorTest3")
    if (currentCount < countToFetch) {
      currentCount = currentCount + 1;
      new gInt(stream.nextInt())
    } else
      new none()
  }
}
