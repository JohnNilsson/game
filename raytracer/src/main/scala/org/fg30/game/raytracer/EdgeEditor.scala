package org.fg30.raytracer

import _root_.java.awt.Color._
import _root_.java.awt.geom.AffineTransform
import _root_.scala.swing.event._
import _root_.scala.swing._
import _root_.javax.swing.{JColorChooser}
import _root_.java.awt.{Point => AWTPoint, Dimension, Graphics2D, BasicStroke, Image}
import _root_.scala.actors.Futures.future

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 06/01/2010
 * Time: 12:15:24 PM
 * To change this template use File | Settings | File Templates.
 */

class ColorPicker(parent: EdgeEditor) extends Component {
  var color = WHITE

  preferredSize = new Dimension(20, 20)

  def update() {
    val newColor = JColorChooser.showDialog(null, "Choose Color", color);

    if (newColor != null) color = newColor

    repaint
  }

  override def paintComponent(g: Graphics2D) = {
    g.setColor(color)
    g.fillRect(0, 0, size.width, size.height)
  }
}

class ToolPanel(parent: EdgeEditor) extends BoxPanel(Orientation.Horizontal) {
  contents += parent.picker
  contents += parent.isEmitter
  contents += parent.renderButton
}

class EditorPanel(parent: EdgeEditor) extends Component {
  preferredSize = new Dimension(800, 800)

  override def paintComponent(g: Graphics2D) = {
    def drawEdge(e: Edge) = {
      val p = e.point
      val x1 = p.x.toInt
      val y1 = p.y.toInt
      val x2 = x1 + e.vector.x.toInt
      val y2 = y1 + e.vector.y.toInt

      if (e.emitter) {
        g.setStroke(new BasicStroke(3.0f))
        g.setColor(BLUE)
        g.drawLine(x1, y1, x2, y2)
      }

      g.setStroke(new BasicStroke(1.5f))
      g.setColor(e.color.toAwt)

      parent.selected.foreach(s => {
        if (s == e) {
          g.setColor(GREEN)
        }
      })

      g.drawLine(x1, y1, x2, y2)
    }

    def drawPoint(p: Point) = {
      val x = p.x.toInt
      val y = p.y.toInt

      g.setColor(GREEN)
      g.drawOval(x - 5, y - 5, 10, 10)
    }

    def clear(g: Graphics2D) = {
      g.setColor(BLACK)
		  g.fillRect(0, 0, size.width, size.height)
      g.setStroke(new BasicStroke(1.5f))
    }

    def drawEdges(g: Graphics2D) = {
      parent.edges.foreach(e => {
        drawEdge(e)
      })
    }

    def drawLightmap(g: Graphics2D) = {
      parent.result.foreach(image => {
        val tx = new AffineTransform();
        tx.scale(parent.resolution, parent.resolution);
        g.drawImage(image, tx, null)
      })
    }

    clear(g)
    drawLightmap(g)
    drawEdges(g)

    parent.drag.foreach(e => drawEdge(e))
    parent.nearest.foreach(p => drawPoint(p))
  }
}

class EdgeEditor {
  var edges: List[Edge] = Nil
  val editor = new EditorPanel(this)
  val isEmitter = new CheckBox("Emmiter")
  val renderButton = new Button("Render")
  val picker = new ColorPicker(this)
  val toolPanel = new ToolPanel(this)
  val progress = new ProgressBar()
  var selected: Option[Edge] = None
  var drag: Option[Edge] = None
  
  var start: Option[Point] = None
  var end: Option[Point] = None
  var nearest: Option[Point] = None
  var result: Option[Image] = None

  val resolution = 5
  val maxLength = 1000
  val maxReflections = 5
  val samples = 500
  var render = new Render(0, 0, 800, 800, resolution, maxLength, maxReflections, samples)

  def updateDrag(point: AWTPoint) {
    drag = Some(Edge(start.get, end.get, HDRColor(picker.color), false))
  }

  def findNearest(point: AWTPoint) {
    nearest = None

    val endpoints = edges.map(e => e.point) ::: edges.map(e => e.endpoint) ::: edges.map(e => e.midpoint)

    endpoints.find(p => p.distanceTo(point.x, point.y) < 10).foreach(p => {
      nearest = Some(p)
    })
  }

  def isNearestOnEdge: Boolean = {
    nearest.foreach(p => {
      if (edges.map(e => e.midpoint).contains(p)) {
        return true
      }
    })

    false
  }

  def renderResult = {
    progress.value = 0

    result = Some(render.raytrace(edges,
      (d: Double) => {
        progress.value = (d * 100).toInt
      }))

    progress.value = 100
  }

  def top = new MainFrame {
    title = "Raytracer"
    contents = new BoxPanel(Orientation.Vertical) {
      contents += editor
      contents += toolPanel
      contents += progress
    }
    
    pack
    visible = true
    progress.max = 100

    listenTo(editor.mouse.clicks)
    listenTo(editor.mouse.moves)
    listenTo(picker.mouse.clicks)
    listenTo(isEmitter)
    listenTo(renderButton)

    reactions += {
      case MousePressed(`editor`, point, mod, clicks, pops) => {
        if (isNearestOnEdge) {
          nearest.foreach(p => {
            edges.foreach(e => {
              if (e.midpoint == p) {
                selected = Some(e)
              }
            })
          })

          start = None

          editor.repaint
        } else {
          start = Some(nearest.getOrElse(Point(point.x, point.y)))
        }
      }
      case MouseDragged(`editor`, point, mod) => {
        if (start.isDefined) {
          findNearest(point)

          end = Some(nearest.getOrElse(Point(point.x, point.y)))

          updateDrag(point)

          editor.repaint
        }
      }
      case MouseReleased(`editor`, point, mod, clicks, pops) => {
        if (start.isDefined) {
          updateDrag(point)

          if (drag.isDefined) {
            selected = Some(drag.get)
            edges ::= drag.get
          }

          findNearest(point)

          editor.repaint
        }
      }
      case MouseMoved(`editor`, point, mod) => {
        findNearest(point)

        editor.repaint
      }
      case MouseClicked(`picker`, point, mod, clicks, pops) => {
        picker.update
      }
      case ButtonClicked(`isEmitter`) => {
        selected.foreach(s => {
          edges = edges.filterNot(e => e == s)
          edges ::= Edge(s.point, s.endpoint, s.color, isEmitter.selected)
        })

        editor.repaint
      }
      case ButtonClicked(`renderButton`) => {
        future {
          renderResult

          editor.repaint
        }
      }
    }
  }

  top
}

object EdgeEditorMain {
  def main(args: Array[String]) {
    new EdgeEditor
  }
}