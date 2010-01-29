package org.fg30.game.misc.vector

import _root_.org.scalatest.matchers.ShouldMatchers
import _root_.org.scalatest.FlatSpec
import _root_.org.fg30.misc.vector.Vector2D
import _root_.scala.Math.{sqrt, sin, cos, toRadians}

class Vector2DSpec extends FlatSpec with ShouldMatchers {
  "A Vector" should "be able to be multiplied by an integer" in {
    (new Vector2D(1, 1) * 2).length should equal (sqrt(2) * 2)
  }

  it should "be able to be divided by an integer" in {
    (new Vector2D(1, 1) / 2).length should equal (sqrt(2) / 2)
  }

  it should "be able to be added to another vector" in {
    new Vector2D(1, 0) + new Vector2D(0, 1) should equal (new Vector2D(1, 1))
  }

  it should "be able to be subtracted to another vector" in {
    new Vector2D(1, 1) - new Vector2D(1, 0) should equal (new Vector2D(0, 1)) 
  }

  it should "be able to provide its length" in {
    new Vector2D(1, 1).length should equal (sqrt(2))
  }

  it should "be able to provide its angle" in {
    for (degrees <- -180 until (180, 45)) {
      val rad = toRadians(degrees)
      new Vector2D(cos(rad), sin(rad)).angle should be (rad plusOrMinus 0.01)
    }
  }

  it should "be able to be normalized to a unit vector" in {
    new Vector2D(1, 1).normalized.length should be (1.0 plusOrMinus 0.01)
  }

  it should "be able to return its normal vector" in {
    new Vector2D(0, 1).normal should equal (new Vector2D(-1, 0))
    new Vector2D(1, 0).normal should equal (new Vector2D(0, 1))
  }

  it should "be able to return the dot product of itself and another vector" in {
    new Vector2D(1, 1).dot(new Vector2D(1, 0)) should equal (1)
    new Vector2D(-1, -1).dot(new Vector2D(0, 1)) should equal (-1)
  }

  it should "be able to return the reflection of itself upon another vector" in {
    val r = new Vector2D(1, 0).reflectOn(new Vector2D(1, 1))
    r.x should be (0.0 plusOrMinus 0.01)
    r.y should be (1.0 plusOrMinus 0.01)
  }
}