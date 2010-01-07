package org.fg30.raytracer


import _root_.org.junit.Test

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 01/11/2009
 */

class GridTest {
	def equals(g1: Grid[Int], g2: Grid[Int]): Boolean = {
		for {
			i <- 0 until g1.size
			j <- 0 until g1.size
		} {
			if (g1.get(i, j) != g2.get(i, j)) {
				return false
			}
		}

		true
	}

	@Test
	def testIsDefined() = {
		val g = new Grid[Int](2)
		val v = 1

		assert(g.isDefined(0, 0) == false)

		g.put(0, 0, v)

		assert(g.isDefined(0, 0))
		assert(g.get(0, 0).get == v)
	}

	@Test
	def testMergeSameSize() = {
		val g1 = Grid(List(1, 2, 3, 4))
		val g2 = Grid(List(5, 6, 7, 8))
		val g3 = Grid(List(6, 8, 10, 12))

		g1.merge(g2, _.getOrElse(0) + _.getOrElse(0))

		assert(equals(g1, g3))
	}

	@Test
	def testMergeOtherSizes() = {
		val g1 = Grid(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
		val g2 = Grid(List(1, 2, 3, 4))
		val g3 = Grid(List(1, 2, 3, 4, 6, 8, 7, 11, 13))

		g1.merge(g2, _.getOrElse(0) + _.getOrElse(0), 1, 1)

		assert(equals(g1, g3))
	}

	@Test
	def testMergeOverlap() = {
		val g1 = Grid(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
		val g2 = Grid(List(1, 2, 3, 4))
		val g3 = Grid(List(5, 2, 3, 4, 5, 6, 7, 8, 9))

		g1.merge(g2, _.getOrElse(0) + _.getOrElse(0), -1, -1)

		assert(equals(g1, g3))
	}

	@Test
	def testMergeOverlap2() = {
		val g1 = Grid(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
		val g2 = Grid(List(1, 2, 3, 4))
		val g3 = Grid(List(1, 2, 3, 4, 5, 6, 7, 8, 10))

		g1.merge(g2, _.getOrElse(0) + _.getOrElse(0), 2, 2)

		assert(equals(g1, g3))
	}
}