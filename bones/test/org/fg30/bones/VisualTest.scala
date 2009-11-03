package org.fg30.bones


import _root_.scala.Math._
import _root_.java.awt.Color._
import _root_.scala.swing._
import _root_.scala.swing.event.ButtonClicked
import _root_.java.awt.Graphics

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 24/10/2009
 */

class BoneView() extends Component {
	val colors = List(BLUE, GREEN, YELLOW, ORANGE, RED, MAGENTA)
	val radius = 3

	preferredSize_=(520, 400)

	override def paintComponent(g: Graphics) = {
		def paintJoint(c: Coordinate) {
			g.drawOval(c.x.toInt - radius, c.y.toInt - radius, radius * 2, radius * 2)
		}

		def drawLine(c1: Coordinate, c2: Coordinate) = {
			g.drawLine(c1.x.toInt, c1.y.toInt, c2.x.toInt, c2.y.toInt)
		}

		def paintBone(b: Bone, i: Int) {
			val c1 = b.primary.c
			val c2 = b.secondary.c

			b.children.foreach(paintBone(_, i + 1))

			g.setColor(colors(i % colors.length))
			paintJoint(c1)
			drawLine(c1.move(b.angle - Pi / 2, radius), c2)
			drawLine(c1.move(b.angle + Pi / 2, radius), c2)
		}

		g.setColor(BLACK)
		g.fillRect(0, 0, size.width, size.height)
		paintBone(VisualTest.bone, 0)
	}
}

object VisualTest extends SimpleGUIApplication {
	var bone = TreeBuilder.newTree(260, 380, TreeBuilder.LARGE)

	def top = new MainFrame {
		title = "Tree, size: " + bone.size
		
		val buttonLeft = new Button { text = "<<" }
		val buttonRight = new Button { text = ">>" }

		contents = new BoxPanel(Orientation.Vertical) {
			contents += new BoneView()
			contents += new BoxPanel(Orientation.Horizontal) {
				contents += buttonLeft
				contents += buttonRight
			}
		}

		listenTo(buttonLeft)
		listenTo(buttonRight)
		
		reactions += {
			case ButtonClicked(`buttonLeft`) => {
				bone.rotate(- Pi / 16)
				repaint
			}
			case ButtonClicked(`buttonRight`) => {
				bone.rotate(Pi / 16)
				repaint
			}
		}
	}
}