package org.fg30.bones

import Math._

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 25/10/2009
 */

object TreeBuilder {
	val LARGE = List(80, 60, 50, 45, 40, 30, 20, 15, 10)
	val SMALL = List(120, 100, 80, 40)

	private val spread = Pi / 6

	def newTree(x: Double, y: Double, lengths: List[Int]) = {
		def newBranch(base: Joint, a: Double, lengths: List[Int]): Bone = {
			val other = Joint(base.c.move(a, lengths.head))
			var children = List[Bone]()

			if (lengths.tail.headOption.isDefined) {
				children =
								newBranch(other, a - spread, lengths.tail) ::
								newBranch(other, a + spread, lengths.tail) :: Nil
			}

			new Bone(base, other, children)
		}

		newBranch(Joint(x, y), 3 * Pi / 2, lengths)
	}
}