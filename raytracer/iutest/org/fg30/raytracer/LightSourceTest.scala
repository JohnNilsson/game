package org.fg30.raytracer

import _root_.java.awt.Graphics
import _root_.java.awt.Color._
import _root_.java.awt.{Color => AWTColor}
import _root_.scala.swing._
import _root_.javax.swing.SwingUtilities
import _root_.org.scalatest.junit.AssertionsForJUnit
import _root_.org.junit.Test
import _root_.java.util.Random

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 02/11/2009
 * Time: 9:06:08 PM
 * To change this template use File | Settings | File Templates.
 */

class LightSourceView(dim: Int) extends Component {
  implicit def toAWT(o: Option[Color]) = {
    if (o.isDefined) {
      val c = o.get
      new AWTColor(c.r.toFloat, c.g.toFloat, c.b.toFloat)
    } else {
      BLACK
    }
  }

  preferredSize_=(400, 400)
  var base = new Grid[Color](dim)
  val black: Color = new Color(0.0, 0.0, 0.0)
  val random = new Random
  val clipping = Grid[Boolean](dim, (x: Int, y: Int) => random.nextDouble > 0.95)

  def update(sources: List[LightSource]) = {
    base = Grid[Color](dim, (x: Int, y: Int) => new Color(0.0, 0.0, 0.0))

    sources.foreach(s => {
      base.merge(s.render(clipping), _.getOrElse(black) + _.getOrElse(black), s.x - s.radius, s.y - s.radius)
    })
  }

	override def paintComponent(g: Graphics) = {
    g.setColor(BLACK)
		g.fillRect(0, 0, size.width, size.height)

    val s = size.width / base.size

    for {
      i <- 0 until base.size
      j <- 0 until base.size
    } {
      if (base.isDefined(i, j)) {
        g.setColor(base.get(i, j))
        g.fillRect(i * s, j * s, s, s)
      }

      if (clipping.get(i, j).getOrElse(false)) {
        g.setColor(WHITE)
        g.fillRect(i * s, j * s, s, s)
      }
    }
	}
}

class LightSourceTest extends GUIApplication {
  implicit def fromAWT(c: AWTColor) =
    new Color(c.getRed.toDouble / 255, c.getGreen.toDouble / 255, c.getBlue.toDouble / 255)

  def top = new MainFrame {
		title = "Light Source Test"

    val view = new LightSourceView(40)
    view.update(List(
      new LightSource(20, 10, 10, RED),
      new LightSource(20, 30, 20, GREEN),
      new LightSource(20, 20, 30, BLUE)))

    contents = new BoxPanel(Orientation.Vertical) {
			contents += view
    }
  }

  init
  top.pack
  top.visible = true
}

class Runner extends AssertionsForJUnit {
  @Test
  def testRun = {
    new LightSourceTest

    Thread.sleep(20000)

    assert(true)
  }
}