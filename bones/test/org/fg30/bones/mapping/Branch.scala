package org.fg30.bones.mapping


import java.awt.Image

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 29/10/2009
 */

class Mapping(val offset: Coordinate, val image: Image) {}

class Branch(val bone: Bone, val offset: Coordinate, val image: Image) {}

object Branch {
	def apply(bone: Bone, mapping: Mapping) = {
		new Branch(bone, mapping.offset, mapping.image)
	}
}