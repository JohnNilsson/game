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

/*
case class Color(val r: Double, val g: Double, val b: Double) {
  def *(f: Double) = {
    assert(f >= 0.0)
    new Color(
      min(r * f, 1.0),
      min(g * f, 1.0),
      min(b * f, 1.0))
  }

  def *(c: Color) = {
    new Color(
      r * c.r,
      g * c.g,
      b * c.b)
  }

  def +(c: Color) = {
    new Color(
      r + c.r * (1.0 - r),
      g + c.g * (1.0 - g),
      b + c.b * (1.0 - b))
  }

  def toAwt() = {
    new AWTColor(r.toFloat, g.toFloat, b.toFloat)
  }
  
  require(r >= 0.0 && r <= 1.0)
  require(g >= 0.0 && g <= 1.0)
  require(b >= 0.0 && b <= 1.0)
}
*/
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