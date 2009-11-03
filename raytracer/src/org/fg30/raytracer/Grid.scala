package org.fg30.raytracer

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 01/11/2009
 */

class Grid[T](val size: Int) {
	val data = new Array[Array[T]](size, size)

	def put(x: Int, y: Int, value: T) = data(x)(y) = value

	def get(x: Int, y: Int): Option[T] = {
    if (isDefined(x, y)) Some(data(x)(y)) else None
  }

	def isDefined(x: Int, y: Int): Boolean = {
    if (x < 0 || x >= size || y < 0 || y >= size) return false
    
    data(x)(y).isInstanceOf[T]
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
}

object Grid {
	def apply[T](list: List[T]): Grid[T] = {
		val size = Math.sqrt(list.size)
		val g = new Grid[T](size)

		list.indices.foreach(i => g.put(i % size, i / size, list(i)))

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