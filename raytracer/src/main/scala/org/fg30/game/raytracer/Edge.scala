package org.fg30.raytracer

import _root_.org.fg30.misc.vector.Vector2D
import _root_.java.awt.geom.Line2D
import _root_.java.awt.image.BufferedImage
import _root_.org.fg30.misc.utility.Timer
import _root_.scala.util.Random
import _root_.scala.actors.Futures._
import actors.Future
import java.util.concurrent.{Callable, Executors}

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 26/11/2009
 * Time: 10:52:05 PM
 * To change this template use File | Settings | File Templates.
 */

object ParameterForm {
  def toParameterForm(point: Point, vector: Vector2D) = {
    val x2 = point.x + vector.x
    val y2 = point.y + vector.y

    val a = y2 - point.y
    val b = point.x - x2
    val c = a * point.x + b * point.y

    (a, b, c)
  }

  def apply(point: Point, vector: Vector2D) = {
    toParameterForm(point, vector)
  }
}

class Ray(val point: Point, val angle: Double) {
  val length = 1000
  val vector = new Vector2D(Math.cos(angle) * length, Math.sin(angle) * length)
  val line = Edge.toLine(toEdge)

  def intersects(edge: Edge) = {
    line.intersectsLine(edge.line)
  }

  def intersectionPoint(edge: Edge): Option[Point] = {
    val p1 = ParameterForm(point, vector)
    val p2 = edge.parameterForm

    val det = p1._1 * p2._2 - p2._1 * p1._2

    if (det == 0) {
      None
    } else {
      val x = (p2._2 * p1._3 - p1._2 * p2._3) / det
      val y = (p1._1 * p2._3 - p2._1 * p1._3) / det

      Some(new Point(x, y))
    }
  }

  def reflect(edge: Edge) = {
    new Ray(intersectionPoint(edge).getOrElse(edge.midpoint), vector.reflect(edge.vector).angle + Ray.diffuse)
  }

  def toEdge = {
    new Edge(point, vector, new HDRColor(0.0, 0.0, 0.0), false)
  }
}

object Ray {
  val random = new Random
  val spread = Math.Pi / 4

  def diffuse = {
    random.nextDouble * 2.0 * spread - spread
  }

  def trace(ray: Ray, edges: Seq[Edge], maxLength: Int, i: Int): List[Point] = {
    var closestDistance = java.lang.Double.MAX_VALUE
    var result: Option[Ray] = None

    edges.filter(e => ray.intersects(e)).foreach(e => {
      val o = ray.intersectionPoint(e)

      o.foreach(p => {
        val d = p.distanceTo(ray.point)

        if (d < closestDistance && d > 0.01) {
          closestDistance = d
          result = Some(ray.reflect(e))
        }
      })
    })

    if (result.isDefined && i > 0 && maxLength > 0) {
      result.get.point :: trace(result.get, edges, maxLength - closestDistance.toInt, i - 1)
    } else {
      Nil
    }
  }

  def render(color: HDRColor, ray: Ray, edges: Seq[Edge], maxLength: Int, i: Int): Color = {
    var closestDistance = java.lang.Double.MAX_VALUE
    var closestEdge: Option[Edge] = None
    var result: Option[Ray] = None

    edges.filter(e => ray.intersects(e)).foreach(e => {
      val o = ray.intersectionPoint(e)

      o.foreach(p => {
        val d = p.distanceTo(ray.point)

        if (d < closestDistance && d > 0.01) {
          closestDistance = d
          closestEdge = Some(e)
          result = Some(ray.reflect(e))
        }
      })
    })

    if (result.isDefined && maxLength > closestDistance) {
      if (closestEdge.get.emitter) {
        return color * closestEdge.get.color
      } else if (i > 0) {
        return render(color * closestEdge.get.color, result.get, edges, maxLength - closestDistance.toInt, i - 1)
      }
    }

    new Color(0.0, 0.0, 0.0)
  }
}

case class Point(x: Double, y: Double) {
  def offset(dx: Double, dy: Double) = {
    new Point(x + dx, y + dy)
  }

  def distanceTo(ox: Double, oy: Double): Double = {
    val dx = ox - x
    val dy = oy - y

    Math.sqrt(dx * dx + dy * dy)
  }

  def distanceTo(other: Point): Double = {
    distanceTo(other.x, other.y)
  }

  def angleTo(ox: Double, oy: Double): Double = {
    val dx = ox - x
    val dy = oy - y

    Math.atan2(dy, dx)
  }

  def angleTo(other: Point): Double = {
    angleTo(other.x, other.y)
  }

  def unapply: Tuple2[Double, Double] = {
    (x, y)
  }
}

object Point {
  def apply(x: Int, y: Int) = {
    new Point(x, y)
  }
}

class Edge(val point: Point, val vector: Vector2D, val color: HDRColor, val emitter: Boolean) {
  val line = Edge.toLine(this)
  val parameterForm = ParameterForm(point, vector)

  def midpoint: Point = {
    point.offset(vector.x / 2, vector.y / 2)
  }

  def endpoint: Point = {
    point.offset(vector.x, vector.y)
  }
}

object Edge {
  def apply(p1: Tuple2[Double, Double], p2: Tuple2[Double, Double], color: HDRColor, emitter: Boolean): Edge = {
    val point = new Point(p1._1, p1._2)
    val vector = new Vector2D(p2._1 - p1._1, p2._2 - p1._2)

    new Edge(point, vector, color, emitter)
  }
  
  def apply(p1: Point, p2: Point, color: HDRColor, emitter: Boolean): Edge = {
    apply((p1.x, p1.y), (p2.x, p2.y), color, emitter)
  }

  def toLine(e: Edge) = {
    val t1 = (e.point.x, e.point.y)
    val t2 = (e.point.x + e.vector.x, e.point.y + e.vector.y)

    new Line2D.Double(t1._1, t1._2, t2._1, t2._2)
  }
}

class Progress(complete: Int) {
  var count: Int = 0
  var oldProgress: Int = 0
  
  def update() {
    count += 1
    
    val newProgress = (count * 100.0 / complete).toInt
    
    if (newProgress > oldProgress + 5) {
      println(newProgress + "%")

      oldProgress = newProgress
    }
  }
}

class Render(x1: Int, y1: Int, x2: Int, y2: Int, resolution: Int, maxLength: Int, maxReflections: Int, iterations: Int) {
  val random = new Random

  def raytrace(edges: Seq[Edge]) = {
    val t = new Timer
    val sx = (x2 - x1) / resolution
    val sy = (y2 - y1) / resolution
    val result = new BufferedImage(sx, sy, BufferedImage.TYPE_INT_RGB)
    val progress = new Progress(sx * sy)

    for {
      ix <- 0 until sx
      iy <- 0 until sy
    } {
      val colors = (0 until iterations).map(i => {
        val cx = x1 + ix * resolution + random.nextDouble * resolution
        val cy = y1 + iy * resolution + random.nextDouble * resolution

        Ray.render(
          new HDRColor(1.0, 1.0, 1.0),
          new Ray(new Point(cx, cy), random.nextDouble * Math.Pi * 2),
          edges, maxLength, maxReflections)
      })
      val color = Color.mix(colors)

      result.setRGB(ix, iy, color.toAwt.getRGB)

      progress.update
    }

    println("Raytracing took " + t.elapsed + " ms")
    
    result
  }
}