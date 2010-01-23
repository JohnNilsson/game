package org.fg30.game.tiles

import org.fg30.raytracer.Grid
import collection.mutable.HashMap

/**
 * Created by IntelliJ IDEA.
 * User: joakim
 * Date: 15/01/2010
 * Time: 11:20:32 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class Transition
case class NorthWest() extends Transition
case class North() extends Transition
case class NorthEast() extends Transition
case class West() extends Transition
case class East() extends Transition
case class SouthWest() extends Transition
case class South() extends Transition
case class SouthEast() extends Transition
case class Full() extends Transition
case class InvNorthWest() extends Transition
case class InvNorthEast() extends Transition
case class InvSouthWest() extends Transition
case class InvSouthEast() extends Transition

case class Tile[T](transition: Transition, t: T)

object TileTransitions {
  val deltaMapping: Map[Tuple2[Int, Int], Transition] = Map(
    (-1, -1) -> NorthWest(), (0, -1) -> North(), (1, -1) -> NorthEast(), (-1, 0) -> West(),
    (1, 0) -> East(), (-1, 1) -> SouthWest(), (0, 1) -> South(), (1, 1) -> SouthEast())

  val mapping: Map[List[Transition], Transition] = Map(
    List(NorthWest()) -> NorthWest(),
    List(North()) -> North(),
    List(NorthEast()) -> NorthEast(),
    List(West()) -> West(),
    List(East()) -> East(),
    List(SouthWest()) -> SouthWest(),
    List(South()) -> South(),
    List(SouthEast()) -> SouthEast(),
    List(West(), North()) -> InvSouthEast(),
    List(North(), East()) -> InvSouthWest(),
    List(East(), South()) -> InvNorthWest(),
    List(South(), West()) -> InvNorthEast(),
    List(North(), West(), East(), South()) -> Full())

  var cleaning: Map[Transition, List[Transition]] = Map(
    North() -> List(NorthWest(), NorthEast()),
    West() -> List(NorthWest(), SouthWest()),
    East() -> List(NorthEast(), SouthEast()),
    South() -> List(SouthWest(), SouthEast()),
    InvNorthWest() -> List(East(), SouthEast(), South()),
    InvNorthEast() -> List(West(), SouthWest(), South()),
    InvSouthWest() -> List(East(), NorthEast(), North()),
    InvSouthEast() -> List(West(), NorthWest(), North()),
    Full() -> List(NorthWest(), North(), NorthEast(), West(), East(), SouthWest(),
      South(), SouthEast(), InvNorthWest(), InvNorthEast(), InvSouthWest(), InvSouthEast()))

  def apply[T](grid: Grid[T]): Grid[Seq[Tile[T]]] = {
    def clean(list: List[Transition]) = {
      list.filter(e => {
        list.map(t => cleaning.get(t).getOrElse(List()).contains(e)).foldLeft(false) {_ || _} != true
      })
    }

    def determine(grid: Grid[T], u: T, x: Int, y: Int): Seq[Tile[T]] = {
      if (grid.get(x, y).get == u) return List(Tile[T](Full(), u))

      val detected = deltaMapping.filterKeys(t => {
        val (dx, dy) = t
        val o = grid.get(x + dx, y + dy)
        o.isDefined && o.get == u
      }).valuesIterator.toList

      val raw = mapping.filterKeys(list => {
        list.map(detected.contains(_)).reduceLeft({_ && _})
      }).valuesIterator

      clean(raw.toList).map(new Tile[T](_, u)).toSeq
    }

    val result = new Grid[Seq[Tile[T]]](grid.size)
    val unique = grid.flatMap.removeDuplicates

    for {
      y <- 0 until grid.size
      x <- 0 until grid.size
    } {
      result.put(x, y, unique.map(determine(grid, _, x, y)).flatten)
    }

    result
  }
}