import Game.GridModel.{Hit, Miss, Sunk}
import org.scalatest._
import Game.{Gridson, Position}

class GridTest extends FlatSpec {

  "Position" should "have correctly working == operator" in {
    assert(Position(1,1) == Position(1,1) === true)
    assert(Position(2,1) == Position(1,2) === false)
  }

  "Grid" should "correctly validate positions" in {
    assert(Gridson.empty.validatePosition(Position(2,2)) === true)
    assert(Gridson.empty.validatePosition(Position(0,0)) === true)
    assert(Gridson.empty.validatePosition(Position(5,0)) === true)
    assert(Gridson.empty.validatePosition(Position(7,9)) === true)
    assert(Gridson.empty.validatePosition(Position(10,5)) === false)
    assert(Gridson.empty.validatePosition(Position(15,12)) === false)
    assert(Gridson.empty.validatePosition(Position(-5,0)) === false)
  }

  "Grid" should "correctly change status of fields" in {
    val grid = Gridson.empty
    assert(grid.hitCounter() === 0)
    assert(grid.addStatus(Seq(Position(1,1)), Hit).hitCounter() === 1)
    assert(grid.addStatus(Seq(Position(1,1), Position(1,5)), Hit).hitCounter() === 2)
    assert(grid.addStatus(Seq(Position(1,1)), Hit).newStatus(Position(1,1), Miss).hitCounter === 0)
  }

  "Grid" should "correctly count hits and sinks" in {
    val grid = Gridson.empty
    assert(grid.addStatus(Seq(Position(1,1), Position(1,2)), Sunk).addStatus(Seq(Position(3,3)), Hit).hitAndSinkCounter === 3)
  }

  "Grid" should "correctly return hit positions" in {
    val grid = Gridson.empty
    assert(grid.addStatus(Seq(Position(1,1), Position(1,2)), Sunk).addStatus(Seq(Position(3,3)), Hit).hitPositions === List(Position(3,3)))
  }

}
