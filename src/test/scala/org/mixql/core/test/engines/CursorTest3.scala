package org.mixql.core.test.engines

import org.mixql.core.context.mtype._
import org.mixql.core.logger.logInfo

import scala.util.Random

class CursorTest3() extends MCursorBase {

  val countToFetch = 10;
  var currentCount = 0;

  var stream: Random = null

  override def close(): MBool = {
    logInfo("close was triggered in CursorTest3")
    MBool.True
  }

  override def open(): MBool = {
    logInfo("open was triggered in CursorTest3")
    if (stream == null)
      stream = new scala.util.Random

    MBool.True
  }

  override def fetch(): MType = {
    logInfo("fetch was triggered in CursorTest3")
    if (currentCount < countToFetch) {
      currentCount = currentCount + 1;
      new MInt(stream.nextInt())
    } else
      MNone.get()
  }
}
