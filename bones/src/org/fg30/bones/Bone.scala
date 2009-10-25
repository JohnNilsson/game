package org.fg30.bones

import Math._

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 24/10/2009
 */

class Coordinate(val x: Double, val y: Double) {
	/**
	 * Distance to other coord
	 */
	def distanceTo(other: Coordinate) = {
		val dx = other.x - x
		val dy = other.y - y
		sqrt(dx * dx + dy * dy)
	}

	/**
	 * Angle between two coordinates
	 */
	def angleTo(other: Coordinate) = {
		atan2(
			other.y - y,
			other.x - x)
	}

	/**
	 * Return offset clone
	 * @param a - angle
	 * @param d - distance
	 */
	def move(a: Double, d: Double) = {
		new Coordinate(x + cos(a) * d, y + sin(a) * d)
	}
}

class Joint(var c: Coordinate) {
	def distanceTo(other: Joint) = c.distanceTo(other.c)
	def angleTo(other: Joint) = c.angleTo(other.c)

	/**
	 * Move instance
	 */
	def move(a: Double, d: Double) = {
		c = c.move(a, d)
	}

	/**
	 * Rotate instance
	 */
	def rotate(o: Joint, b: Double) = {
		c = o.c.move(o.angleTo(this) + b, distanceTo(o))
	}

	/**
	 * Return rotated clone
	 */
	def rotate2(o: Joint, b: Double) = {
		new Joint(o.c.move(o.angleTo(this) + b, distanceTo(o)))
	}
}

object Joint {
	def apply(c: Coordinate) = new Joint(c)
	def apply(x: Double, y: Double) = new Joint(new Coordinate(x, y))
}

class Bone(val primary: Joint, val secondary: Joint, val children: List[Bone]) {
	def length = primary.distanceTo(secondary)
	def angle = primary.angleTo(secondary)

	/**
	 * Rotate instance
	 */
	def rotate(b: Double): Unit = {
		rotate(primary, b)
	}

	/**
	 * Rotate instance
	 */
	def rotate(o: Joint, b: Double): Unit = {
		secondary.rotate(o, b)
		children.foreach(_.rotate(o, b))
	}

	/**
	 * Return rotated clone
	 */
	def rotate2(b: Double): Bone = {
		rotate2(primary, primary, b)
	}

	/**
	 * Return rotated clone
	 */
	def rotate2(o: Joint, p: Joint, b: Double): Bone = {
		val sec2 = secondary.rotate2(o, b)
		new Bone(p, sec2, children.map(_.rotate2(o, sec2, b)))
	}

	/**
	 * Return recursive size of Bone tree, children included
	 */
	def size: Int = {
		children.foldLeft(1)(_ + _.size)
	}
}