package edu.umassmed.dmetag.utils

import scala.concurrent.{Promise, Future}

/**
 * Some general purpose utility methods
 */
object Utils {
  /**
   * Delay for a given time
   * @param milliseconds # of milliseconds to delay
   * @return future that completes after specified time
   */
  def delay(milliseconds: Int): Future[Unit] = {
    val p = Promise[Unit]()
    scala.scalajs.js.timers.setTimeout(milliseconds) {
      p.success(())
    }
    p.future
  }
}
