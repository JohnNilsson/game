package org.fg30.misc.vector

import _root_.scala.Math.{sin, cos}

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 03/12/2009
 * Time: 4:37:40 PM
 * To change this template use File | Settings | File Templates.
 */

case class Vector2D(x: Double, y: Double) {
  def length = {
    Math.sqrt(x * x + y * y)
  }

  def angle = {
    Math.atan2(y, x)
  }

  def -(other: Vector2D) = {
    new Vector2D(x - other.x, y - other.y)
  }

  def +(other: Vector2D) = {
    new Vector2D(x + other.x, y + other.y)
  }

  def *(d: Double) = {
    new Vector2D(x * d, y * d)
  }

  def /(d: Double) = {
    new Vector2D(x / d, y / d)
  }

  def normalized = {
    this/length
  }

  def normal = {
    new Vector2D(-y, x)
  }

  def dot(other: Vector2D) = {
    x * other.x + y * other.y
  }

  def reflectOn(other: Vector2D) = {
    val n = other.normal.normalized

    this - n * 2 * (n dot this)
  }

  def rotate(a: Double) = {
    new Vector2D(cos(angle + a) * length, sin(angle + a) * length)
  }
}