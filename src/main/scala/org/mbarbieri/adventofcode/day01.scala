package org.mbarbieri.adventofcode

object Day01 extends App {

  abstract case class Direction(offsetX: Int, offsetY: Int)
  object North extends Direction(0, 1)
  object South extends Direction(0, -1)
  object East extends Direction(1, 0)
  object West extends Direction(-1, 0)

  sealed trait Turn
  object Right extends Turn
  object Left extends Turn

  case class Move(turn: Turn, distance: Int)
  case class Position(cell: Cell, direction: Direction)
  case class Cell(x: Int, y: Int)

  val startingPosition = Position(Cell(0, 0), North)

  def turnTo(currentDirection: Direction, turn: Turn): Direction = {
    (currentDirection, turn) match {
      case (North, Right) => East
      case (North, Left) => West
      case (South, Right) => West
      case (South, Left) => East
      case (East, Right) => South
      case (East, Left) => North
      case (West, Right) => North
      case (West, Left) => South
    }
  }

  def move(currentPosition: Position, move: Move): Position = {
    val newDirection = turnTo(currentPosition.direction, move.turn)
    Position(
      Cell(currentPosition.cell.x + (move.distance * newDirection.offsetX),
        currentPosition.cell.y + (move.distance * newDirection.offsetY)),
      newDirection)
  }

  val moves = List(Move(Right,2),Move(Left,3),Move(Right,2),Move(Right,4),Move(Left,2),Move(Left,1),Move(Right,2),Move(Right,4),Move(Right,1),Move(Left,4),Move(Left,5),Move(Right,5),Move(Right,5),Move(Right,2),Move(Right,2),Move(Right,1),Move(Left,2),Move(Left,3),Move(Left,2),Move(Left,1),Move(Right,3),Move(Left,5),Move(Right,187),Move(Right,1),Move(Right,4),Move(Left,1),Move(Right,5),Move(Left,3),Move(Left,4),Move(Right,50),Move(Left,4),Move(Right,2),Move(Right,70),Move(Left,3),Move(Left,2),Move(Right,4),Move(Right,3),Move(Right,194),Move(Left,3),Move(Left,4),Move(Left,4),Move(Left,3),Move(Left,4),Move(Right,4),Move(Right,5),Move(Left,1),Move(Left,5),Move(Left,4),Move(Right,1),Move(Left,2),Move(Right,4),Move(Left,5),Move(Left,3),Move(Right,4),Move(Left,5),Move(Left,5),Move(Right,5),Move(Right,3),Move(Right,5),Move(Left,2),Move(Left,4),Move(Right,4),Move(Left,1),Move(Right,3),Move(Right,1),Move(Left,1),Move(Left,2),Move(Right,2),Move(Right,2),Move(Left,3),Move(Right,3),Move(Right,2),Move(Right,5),Move(Right,2),Move(Right,5),Move(Left,3),Move(Right,2),Move(Left,5),Move(Right,1),Move(Right,2),Move(Right,2),Move(Left,4),Move(Left,5),Move(Left,1),Move(Left,4),Move(Right,4),Move(Right,3),Move(Right,1),Move(Right,2),Move(Left,1),Move(Left,2),Move(Right,4),Move(Right,5),Move(Left,2),Move(Right,3),Move(Left,4),Move(Left,5),Move(Left,5),Move(Left,4),Move(Right,4),Move(Left,2),Move(Right,1),Move(Right,1),Move(Left,2),Move(Left,3),Move(Left,2),Move(Right,2),Move(Left,4),Move(Right,3),Move(Right,2),Move(Left,1),Move(Left,3),Move(Left,2),Move(Left,4),Move(Left,4),Move(Right,2),Move(Left,3),Move(Left,3),Move(Right,2),Move(Left,4),Move(Left,3),Move(Right,4),Move(Right,3),Move(Left,2),Move(Left,1),Move(Left,4),Move(Right,4),Move(Right,2),Move(Left,4),Move(Left,4),Move(Left,5),Move(Left,1),Move(Right,2),Move(Left,5),Move(Left,2),Move(Left,3),Move(Right,2),Move(Left,2))

  val endPosition = moves.foldLeft(startingPosition)((p, m) => move(p, m))

  val distance = Math.abs(endPosition.cell.x) + Math.abs(endPosition.cell.y)

  println(s"Result is $distance")
}
