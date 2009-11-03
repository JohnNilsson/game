package org.fg30.raytracer

import _root_.scala.Math._

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 02/11/2009
 * Time: 8:15:47 PM
 * To change this template use File | Settings | File Templates.
 */

class Color(val r: Double, val g: Double, val b: Double) {
  def *(f: Double) = {
    assert(f >= 0 && f <= 1.0)
    new Color(r * f, g * f, b * f)
  }

  def +(c: Color) = {
    new Color(
      r + c.r * (1.0 - r),
      g + c.g * (1.0 - g),
      b + c.b * (1.0 - b))
  }
}

class LightSource(val x: Int, val y: Int, val radius: Int, val color: Color) {
  def render(base: Grid[Boolean]): Grid[Color] = {
    def trace(grid: Grid[Color], dx: Int, dy: Int): Unit = {
      def distance(dx: Int, dy: Int) = {
        sqrt(dx * dx + dy * dy)
      }

      val a = atan2(dy, dx)

      for (i <- 0 until 2 * min(distance(dx, dy), radius)) {
        val j = i / 2
        val ix = radius + (cos(a) * j).toInt
        val iy = radius + (sin(a) * j).toInt

        if (base.get(ix + x - radius, iy + y - radius).getOrElse(false)) {
          return
        }

        if (!grid.isDefined(ix, iy)) {
          val f = 1.0 - pow(j.toDouble / radius, 2)
          grid.put(ix, iy, color * f)
        }
      }
    }

    var coordinates: List[Tuple2[Int, Int]] = Nil
    for (i <- -radius until radius) {
      coordinates = (i, -radius) :: (i, radius) :: (-radius, i) :: (radius, i) :: coordinates
    }

    val result = new Grid[Color](2 * radius + 1)
    coordinates.foreach(t => {
      val (ix, iy) = t
      trace(result, ix, iy)
    })

    result
  }
}