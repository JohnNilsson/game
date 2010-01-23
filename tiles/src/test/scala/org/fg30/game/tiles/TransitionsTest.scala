package org.fg30.game.tiles

import _root_.org.junit.Test
import _root_.org.fg30.raytracer.Grid

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 17/01/2010
 * Time: 6:40:28 PM
 * To change this template use File | Settings | File Templates.
 */

class TransitionsTest {
  @Test
  def testTransition() {
    val grid = Grid[Int](3, (x: Int, y: Int) => 0)
    grid.put(1, 1, 1)

    var t = TileTransitions(grid)

    for {
      x <- 0 until t.size
      y <- 0 until t.size
    } {
      println(x + ", " + y + ": " + t.get(x, y))
    }
  }
}