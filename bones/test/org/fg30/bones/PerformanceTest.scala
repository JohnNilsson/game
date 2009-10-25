package org.fg30.bones


import junit.Test
import scalatest.junit.AssertionsForJUnit
import Math._

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 25/10/2009
 */

object Timer {
	var nano: Long = 0L
	var message: String = ""

	def start(m: String) = {
		nano = System.nanoTime
		message = m
	}

	def stop = {
		val passed = System.nanoTime - nano

		println(message + ": " + passed / 1000000 + " ms")
	}
}

class PerformanceTest extends AssertionsForJUnit {
	/**
	 * Estimate performance of rotating instances of Bone
	 */
	@Test
	def rotateInstances = {
		Thread.sleep(10000)

		val tree = TreeBuilder.newTree(0, 0, TreeBuilder.LARGE)
		val step = Pi / 500

		Timer.start("Instances")
		(0 until 10000).foreach(i => tree.rotate(step))
		Timer.stop
	}

	/**
	 * Estimate performance of rotating cloned Bones
	 */
	@Test
	def rotateClone = {
		Thread.sleep(10000)

		var tree = TreeBuilder.newTree(0, 0, TreeBuilder.LARGE)
		val step = Pi / 500

		Timer.start("Clones")
		(0 until 10000).foreach(i => tree = tree.rotate2(tree.primary, tree.primary, step))
		Timer.stop
	}
}