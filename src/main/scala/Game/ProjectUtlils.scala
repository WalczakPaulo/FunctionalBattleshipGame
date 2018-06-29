package Game
import Game.GridModel.{Grid, Undiscovered}
import Game.ShipOrientation.ShipOrientation

object Constants {
  val gridWidth = 10
  val gridHeight = 10
  val rowBorder = "S 0 1 2 3 4 5 6 7 8 9"
  val shipSizesForAI = Seq(2,3,4)
  val numberOfUserBoats = 1
}

object ResponseMessage extends Enumeration{
  val boatPlacingError = "thats a wrong place for this ship"
  val boatPlacedCorrectly = "the ship is ready"
  val alreadyHit = "oops, you already tried to hit it here"
  val sunk = "boom, ship is down!!"
  val hitAttack = "kshksh ... hit!"
  val error = "ooops, it's a wrong input"
  val missedAttack = "sorry, miss!"
}

object Gridson{
  val grid = Seq.fill(Constants.gridHeight)(Seq.fill(Constants.gridWidth)(Undiscovered))
  val empty = Grid(grid)
}

object ShipOrientation extends Enumeration {
  type ShipOrientation = Value
  val Horizontal, Vertical = Value
}

case class Position(x: Int, y: Int) {
  def ==(other: Position): Boolean = {
    x == other.x && y == other.y
  }
}

case class Ship(position: Position, length: Int, orientation: ShipOrientation)