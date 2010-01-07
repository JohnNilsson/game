package org.fg30.misc.utility

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 13/11/2009
 * Time: 9:02:09 PM
 * To change this template use File | Settings | File Templates.
 */

class Timer {
  var nano: Long = 0

  private def toMs(nano: Long) = nano / 1000000

  def elapsed = {
    toMs(System.nanoTime - nano)
  }

  def reset = {
    nano = System.nanoTime
  }

  reset
}

object Timer {
  def now = System.nanoTime / 1000000
}

object FastMath {
  def sqrt(x: Double) = {
    if (x == 0) {
      0
    }

    var root = x / 2
    for (k <- 0 until 5) {
      root = (root + (x / root)) / 2;
    }

    root
  }
}