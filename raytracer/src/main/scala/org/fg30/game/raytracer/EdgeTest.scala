package org.fg30.raytracer

import _root_.java.awt.Color._
import _root_.scala.swing._
import _root_.java.awt.geom.AffineTransform
import _root_.java.awt.image.{AffineTransformOp, RescaleOp}
import _root_.java.awt.{Dimension, Graphics2D, Graphics}

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 03/12/2009
 * Time: 4:47:40 PM
 * To change this template use File | Settings | File Templates.
 */

class EdgeView(parent: EdgeTest) extends Component {
  preferredSize = new Dimension(800, 800)

  def offset(point: Point) = {
    point.offset(size.width / 2, size.height / 2)
  }

  override def paintComponent(g: Graphics2D) = {
    def drawEdge(e: Edge) = {
      val p = offset(e.point)
      val x1 = p.x.toInt
      val y1 = p.y.toInt
      val x2 = x1 + e.vector.x.toInt
      val y2 = y1 + e.vector.y.toInt

      g.drawLine(x1, y1, x2, y2)
    }

    def clear(g: Graphics2D) = {
      g.setColor(BLACK)
		  g.fillRect(0, 0, size.width, size.height)
    }

    def drawLightmap(g: Graphics2D) = {
      val image = parent.render.raytrace(parent.edges)
      val tx = new AffineTransform();
      tx.scale(parent.resolution, parent.resolution);
      g.drawImage(image, tx, null)
    }

    def drawEdges(g: Graphics2D) = {
      parent.edges.foreach(e => {
        g.setColor(e.color.toAwt)
        drawEdge(e)
      })
    }

    clear(g)
    drawLightmap(g)
    drawEdges(g)
  }
}

class EdgeTest {
  val resolution = 5
  val maxLength = 5000
  val maxReflections = 5
  val samples = 100

  var ray = new Ray(new Point(0, 0), Math.Pi / 3)
  var render = new Render(-400, -400, 400, 400, resolution, maxLength, maxReflections, samples)
  val edges =
    // edgeCircle(8, 8, new Point(-250, 0), new Color(1.0, 1.0, 1.0), true) :::
    // edgeCircle(8, 64, new Point(0, -250), new Color(1.0, 0.0, 0.0), true) :::
    // edgeCircle(32, 16, new Point(0, 0), new HDRColor(5.0, 4.8, 4.5), true) :::
    // circles(7, 8, 320, new HDRColor(1.0, 1.6, 2.0), true) :::
    circles(9, 48, 240, new HDRColor(0.4, 0.3, 0.2), false) :::
    circles(12, 16, 150, new HDRColor(0.8, 0.4, 0.2), false) :::
    // edgeCircle(32, 8, new Point(0, 0), new HDRColor(0.0, 0.5, 2.0), true) :::
    // circles(7, 8, 250, new Color(1.0, 1.0, 1.0), true) :::
    Edge((-400.0, 250.0), (400.0, 250.0), new HDRColor(2.0, 3.2, 4.0), true) ::
    edgeCircle(32, 380, new Point(0, 0), new HDRColor(0.6, 0.6, 0.6), false)

  val view = new EdgeView(this)

  def circleOfTuples(count: Int, radius: Int, c: Point) = {
    (0 until count).
            map(i => i * (Math.Pi * 2) / count).
            map(a => (c.x + Math.cos(a) * radius, c.y + Math.sin(a) * radius))
  }

  def circles(count: Int, innerRadius: Int, outerRadius: Int, color: HDRColor, emitter: Boolean) = {
    var result: List[Edge] = Nil

    circleOfTuples(count, outerRadius, new Point(0, 0)).foreach(t => {
      result :::= edgeCircle(16, innerRadius, new Point(t._1, t._2), color, emitter)
    })

    result
  }

  def edgeCircle(count: Int, radius: Int, c: Point, color: HDRColor, emitter: Boolean) = {
    var list: Seq[Tuple2[Double, Double]] = circleOfTuples(count, radius, c)
    var ot = list.last
    var result: List[Edge] = Nil

    list.foreach(t => {
      result ::= Edge(ot, t, color, emitter)
      ot = t
    })

    result
  }

  def top = new MainFrame {
		title = "Edge Test"

    contents = new BoxPanel(Orientation.Vertical) {
			contents += view
    }

    pack
    visible = true
  }

  top
}

object EdgeTestMain {
  def main(args: Array[String]) {
    new EdgeTest
  }
}