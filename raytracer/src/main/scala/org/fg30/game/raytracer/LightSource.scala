package org.fg30.raytracer

import _root_.scala.Math._
import _root_.java.awt.{Color => AWTColor}

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 02/11/2009
 * Time: 8:15:47 PM
 * To change this template use File | Settings | File Templates.
 */

case class Color(val r: Double, val g: Double, val b: Double) {
  def toAwt() = {
    new AWTColor(
      min(r, 1.0).toFloat,
      min(g, 1.0).toFloat,
      min(b, 1.0).toFloat)
  }
}

case class HDRColor(pr: Double, pg: Double, pb: Double) extends Color(pr, pg, pb) {
  def *(c: Color) = {
    new HDRColor(
      r * c.r,
      g * c.g,
      b * c.b)
  }
}

object HDRColor {
  def apply(c: AWTColor) = {
    new HDRColor(c.getRed.toDouble / 255, c.getGreen.toDouble / 255, c.getBlue.toDouble / 255)
  }
}

object Color {
  def mix(colors: Seq[Color]) = {
    var r = 0.0
    var g = 0.0
    var b = 0.0

    colors.foreach(c =>  {
      r += c.r
      g += c.g
      b += c.b
    })

    val s = colors.length

    new Color(r / s, g / s, b / s)
  }
}