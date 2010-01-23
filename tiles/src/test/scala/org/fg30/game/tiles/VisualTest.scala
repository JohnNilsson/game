package org.fg30.game.tiles

import _root_.java.awt.Color._
import _root_.scala.swing._
import _root_.org.fg30.raytracer.Grid
import _root_.scala.util.Random
import _root_.javax.imageio.ImageIO
import _root_.java.net.URL
import _root_.java.io.File
import _root_.java.awt.image.BufferedImage
import _root_.scala.collection.mutable.HashMap
import java.awt.{Color, Image, Graphics2D, Dimension}

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 18/01/2010
 * Time: 8:57:29 PM
 * To change this template use File | Settings | File Templates.
 */

class TileComponent(parent: VisualTest) extends Component {
  preferredSize = new Dimension(parent.mapSize * parent.tileSize, parent.mapSize * parent.tileSize)
  
  val tiles = new HashMap[Tile[Int], Image]
  val backgroundColor = new Color(0.1f, 0.1f, 0.1f)
  val gridColor = new Color(0.0f, 0.0f, 0.0f, 0.25f)

  def initTiles = {
    def addMapping(i: Int, image: BufferedImage) = {
      tiles.put(Tile(SouthEast(), i), image.getSubimage(0, 0, 64, 64))
      tiles.put(Tile(South(), i), image.getSubimage(64, 0, 64, 64))
      tiles.put(Tile(SouthWest(), i), image.getSubimage(128, 0, 64, 64))
      tiles.put(Tile(East(), i), image.getSubimage(0, 64, 64, 64))
      tiles.put(Tile(Full(), i), image.getSubimage(64, 64, 64, 64))
      tiles.put(Tile(West(), i), image.getSubimage(128, 64, 64, 64))
      tiles.put(Tile(NorthEast(), i), image.getSubimage(0, 128, 64, 64))
      tiles.put(Tile(North(), i), image.getSubimage(64, 128, 64, 64))
      tiles.put(Tile(NorthWest(), i), image.getSubimage(128, 128, 64, 64))
      tiles.put(Tile(InvSouthEast(), i), image.getSubimage(192, 0, 64, 64))
      tiles.put(Tile(InvSouthWest(), i), image.getSubimage(256, 0, 64, 64))
      tiles.put(Tile(InvNorthEast(), i), image.getSubimage(192, 64, 64, 64))
      tiles.put(Tile(InvNorthWest(), i), image.getSubimage(256, 64, 64, 64))
    }

    addMapping(1, ImageIO.read(Thread.currentThread.getContextClassLoader.getResourceAsStream("images/tiles1.png")))
    addMapping(2, ImageIO.read(Thread.currentThread.getContextClassLoader.getResourceAsStream("images/tiles2.png")))
  }

  override def paintComponent(g: Graphics2D) = {
    val s = parent.grid.size
    val ts = parent.tileSize

    g.setColor(backgroundColor)
    g.fillRect(0, 0, size.width, size.height)

    for {
      iy <- 0 until s
      ix <- 0 until s
    } {
      parent.transitions.get(ix, iy).get.toList.sort((t1, t2) => t1.t < t2.t).foreach(t => {
        tiles.get(t).foreach(g.drawImage(_, ix * ts, iy * ts, null))
      })

      g.setColor(gridColor)
      g.drawRect(ix * ts, iy * ts, ts, ts)
    }
  }

  initTiles
}

class VisualTest {
  val delta = List((0, -1), (-1, 0), (1, 0), (0, 1))

  def generateGrid = {
    val result = new Grid[Int](mapSize)

    (0 until 20).foreach(i => {
      val (x, y) = (random.nextInt(mapSize), random.nextInt(mapSize))
      result.put(x, y, 2)

      delta.foreach(t => {
        val (dx, dy) = t
        if (!result.isDefined(x + dx, y + dy) && result.isWithin(x + dx, y + dy)) result.put(x + dx, y + dy, 1)
      })
    })

    Grid[Int](mapSize, (x: Int, y: Int) => { result.get(x, y).getOrElse(0)})
  }

  val tileSize = 64
  val mapSize = 10

  val random = new Random
  val grid = generateGrid
  val transitions = TileTransitions(grid)
  val tileComponent = new TileComponent(this)

  def top = new MainFrame {
    title = "Transitions"
    contents = new BoxPanel(Orientation.Vertical) {
      contents += tileComponent
    }

    pack
    visible = true
  }

  top
}

object VisualTestMain {
  def main(args: Array[String]) {
    new VisualTest
  }
}
