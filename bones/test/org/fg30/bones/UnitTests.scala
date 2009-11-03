package org.fg30.bones


import _root_.org.scalatest.junit.AssertionsForJUnit
import _root_.org.junit.Test
import _root_.scala.Math._

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 24/10/2009
 */

class UnitTests extends AssertionsForJUnit {
	/**
	 * Verify that the bone class rotates properly
	 */
	@Test
	def verifyBoneAngle = {
		val primary = Joint(0.0, 0.0)
		
		List(0.0, 0.5, 1.0, 1.5, 2.0).foreach(a => {
			val secondary = Joint(cos(a) * 10.0, sin(a) * 10.0)
			val bone = new Bone(primary, secondary, List())

			assert(bone.angle === a)
		})
	}
}