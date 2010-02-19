package org.fg30.game.misc.bones

import _root_.scala.swing._
import _root_.scala.Math.{sin, cos}
import _root_.scala.math.Pi
import _root_.scala.actors.Actor.actor
import org.fg30.misc.vector.Vector2D
import java.awt.{Polygon, Graphics2D, Dimension}
import java.awt.geom.GeneralPath

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 16/02/2010
 * Time: 11:25:30 PM
 * To change this template use File | Settings | File Templates.
 */

class BoneView(parent: VisualBoneTest, root: Bone) extends Component {
  preferredSize = new Dimension(400, 400)

  override def paintComponent(g: Graphics2D) = {
    def paint(x: Double, y: Double, bone: Bone): Unit = {
      def draw(x: Double, y: Double, v: Vector2D) = {
        val path = new GeneralPath

        path.moveTo(x + v.x, y + v.y)
        path.lineTo(x + 5 * cos(v.angle + Pi / 2), y + 5 * sin(v.angle + Pi / 2))
        path.lineTo(x + 5 * cos(v.angle + Pi), y + 5 * sin(v.angle + Pi))
        path.lineTo(x + 5 * cos(v.angle - Pi / 2), y + 5 * sin(v.angle - Pi / 2))
        path.lineTo(x + v.x, y + v.y)

        g.draw(path)
      }

      draw(x, y, bone.vector)
      
      bone.children.foreach(paint(x + bone.vector.x, y + bone.vector.y, _))
    }

    paint(200, 200, root)
  }
}

class VisualBoneTest {
  val root = Bone(Vector2D(50.0, 0.0), List(
    Bone(Vector2D(50.0, 0.0)),
    Bone(Vector2D(0.0, 50.0))
  ))
  val boneView = new BoneView(this, root)

  def top = new MainFrame {
    title = "Bones"
    contents = new BoxPanel(Orientation.Vertical) {
      contents += boneView
    }

    actor {
      while(true) {
        Bone.recRotate(root, Pi / 16)
        boneView.repaint
        Thread.sleep(100)
      }
    }

    pack
    visible = true
  }

  top
}

object VisualTestMain {
  def main(args: Array[String]) {
    new VisualBoneTest
  }
}