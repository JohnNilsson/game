package org.fg30.game.misc.bones

import org.fg30.misc.vector.Vector2D

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 31/01/2010
 * Time: 2:11:58 PM
 * To change this template use File | Settings | File Templates.
 */


class Node[T](var t: T)

case class Connection[T](node: Node[T], children: List[Node[T]])

class Bone(var vector: Vector2D, val children: List[Bone]) {
  val node = new Node(vector)
  val connection = new Connection(node, children.map(b => b.node))
}

object Bone {
  def apply(vector: Vector2D, children: List[Bone]): Bone = {
    new Bone(vector, children) 
  }

  def apply(vector: Vector2D): Bone = {
    new Bone(vector, Nil)
  }

  def rotate(bone: Bone, a: Double) = {
    bone.vector = bone.vector.rotate(a)
  }

  def recRotate(bone: Bone, a: Double): Unit = {
    rotate(bone, a)
    bone.children.foreach(recRotate(_, a))
  }
}