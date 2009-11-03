package org.fg30.bones


import java.awt.geom.AffineTransform
import java.awt.{Graphics2D, Graphics}
import java.io.File
import javax.imageio.ImageIO
import mapping.Mapping
import Math._
import java.awt.Color._
import swing._
import event.ButtonClicked
/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 24/10/2009
 */

class BoneView() extends Component {
	val image = ImageIO.read(new File("/home/joakim/branch.png"))
	val mapping = new Mapping(new Coordinate(60, 140), image)
	val radius = 3

	preferredSize_=(800, 600)

	override def paintComponent(g: Graphics) = {
		def paintJoint(c: Coordinate) {
			g.drawOval(c.x.toInt - radius, c.y.toInt - radius, radius * 2, radius * 2)
		}

		def drawLine(c1: Coordinate, c2: Coordinate) = {
			g.drawLine(c1.x.toInt, c1.y.toInt, c2.x.toInt, c2.y.toInt)
		}

		def paintBone(b: Bone) {
			val c1 = b.primary.c
			val c2 = b.secondary.c

			b.children.foreach(paintBone(_))

			g.setColor(GREEN)
			paintJoint(c1)
			drawLine(c1.move(b.angle - Pi / 2, radius), c2)
			drawLine(c1.move(b.angle + Pi / 2, radius), c2)
		}

		def paintMapping(bone: Bone): Unit = {
			bone.children.foreach(paintMapping(_))
			
			val f = bone.length / 120
			val t = new AffineTransform()
			t.translate(
				bone.primary.c.x - mapping.offset.x * f,
				bone.primary.c.y - mapping.offset.y * f)
			t.rotate(bone.angle + Pi / 2, mapping.offset.x * f, mapping.offset.y * f)
			t.scale(f, f)

			val g2d = g.asInstanceOf[Graphics2D]
			g2d.drawImage(image, t, null)
		}

		Timer.start("Paint")
		g.setColor(BLUE)
		g.fillRect(0, 0, size.width, size.height)

		val bone = VisualTest.bone
		paintMapping(bone)
		// paintBone(bone)
		Timer.stop
	}
}

object VisualTest extends SimpleGUIApplication {
	val bone = TreeBuilder.newTree(400, 580, TreeBuilder.SMALL)

	def top = new MainFrame {
		title = "Tree, size: " + bone.size

		contents = new BoxPanel(Orientation.Vertical) {
			contents += new BoneView()
		}
	}
}