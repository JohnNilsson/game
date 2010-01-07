package org.fg30.raytracer

import _root_.scala.collection.mutable.GenericArray

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 01/11/2009
 */

class Grid[T](val size: Int) {
  val data = new GenericArray[GenericArray[T]](size)
  data.indices.foreach(i => data(i) = new GenericArray[T](size))

  def isDefined(x: Int, y: Int): Boolean = {
    if (!isWithin(x, y)) return false

    data(x)(y).isInstanceOf[T]
  }

  def isWithin(x: Int, y: Int): Boolean = {
    (x >= 0 && x < size && y >= 0 && y < size)
  }

	def put(x: Int, y: Int, value: T) = data(x)(y) = value

	def get(x: Int, y: Int): Option[T] = {
    if (isDefined(x, y)) Some(data(x)(y)) else None
  }

  override def clone() = {
    val other = new Grid[T](size)

    for {
      i <- 0 until size
      j <- 0 until size
    } {
      other.data(i)(j) = data(i)(j)
    }

    other
  }

  def sub(offset: Tuple2[Int, Int], newSize: Int) = {
    val (ox, oy) = offset

    val other = new Grid[T](newSize)

    for {
      i <- 0 until newSize
      j <- 0 until newSize
    } {
      if (isDefined(ox + i, oy + j)) {
        other.data(i)(j) = data(ox + i)(oy + j)
      }
    }

    other
  }

	def merge(other: Grid[T], f: (Option[T], Option[T]) => T): Unit = {
		merge(other, f, 0, 0)
	}

	def merge(other: Grid[T], f: (Option[T], Option[T]) => T, x: Int, y: Int): Unit = {
		for {
			i <- Math.max(x, 0) until Math.min(other.size + x, size)
			j <- Math.max(y, 0) until Math.min(other.size + y, size)
		} {
			data(i)(j) = f(get(i, j), other.get(i - x, j - y))
		}
	}

  def flatMap(): GenericArray[T] = {
    data.flatMap(x => x)
  }

  def map[S](f: (T) => S) = {
    val other = new Grid[S](size)

    for {
      i <- 0 until size
      j <- 0 until size
    } {
      other.data(i)(j) = f(data(i)(j))
    }

    other
  }
}

object Grid {
	def apply[T](list: List[T]): Grid[T] = {
		val size = Math.sqrt(list.size).toInt
		val g = new Grid[T](size)

		list.indices.foreach((i: Int) => g.put(i % size, i / size, list(i)))

		g
	}

  def apply[T](size: Int, f: (Int, Int) => T): Grid[T] = {
    val g = new Grid[T](size)

    for {
      i <- 0 until size
      j <- 0 until size
    } {
      g.data(i)(j) = f(i, j)
    }

    g
  }
}