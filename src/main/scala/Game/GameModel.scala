package Game
import scala.io.StdIn.readLine
import scala.util.{Failure, Random, Success, Try}
import GridModel._
import Game.ShipOrientation._
import Constants.{shipSizesForAI, numberOfUserBoats}
import Game.Ship

object GameModel {

  sealed trait Player {

    val name: String
    val grid: Grid
    val ships: Seq[Ship]

    def createPlayer(name: String, grid: Grid, ships: Seq[Ship]): Player
    def successfulAttack(p: Position, defender: Player): (Player, Player, String)
    def sunkAttack(p: List[Position], defender: Player): (Player, Player, String)
    def missedAttack(p: Position, defender: Player): (Player, Player, String)
    def alreadyHitAttack(defender: Player): (Player, Player, String)

    def attack(p: Position, defender: Player): (Player, Player, String) = {
      p match{
        case a if !grid.validatePosition(a) => (this, defender, ResponseMessage.error)
        case _ if isHitShip(p, defender.ships) => {
          checkIfSunk(p, defender.ships) match {
            case Some(positions) => sunkAttack(positions, defender)
            case _ if grid.statusAt(p) == Hit ||  grid.statusAt(p) == Sunk => alreadyHitAttack(defender)
            case _ => successfulAttack(p, defender)
          }
        }
        case _ if grid.statusAt(p) == Miss => alreadyHitAttack(defender)
        case _ => missedAttack(p, defender)
      }
    }

    def checkIfSunk(guess: Position, defenderShips: Seq[Ship]): Option[List[Position]] = {
      def helperFunction(positions: Seq[Position]) = {
        positions.forall(pos => grid.statusAt(pos) == Hit || pos == guess)
      }
      defenderShips.map(s => createPositionsFromShip(s)).map(positions => if(helperFunction(positions)) return Some(positions))
      None
    }

    def isHitShip(position: Position, defenderShips: Seq[Ship]): Boolean = {

      def isPointInShip(position: Position, ship: Ship): Boolean = {
        val positionsStream = (0 until ship.length).map { l =>
          ship.orientation match {
            case Horizontal => Position(ship.position.x + l, ship.position.y)
            case _ => Position(ship.position.x, ship.position.y + l)
          }
        }.toStream
        positionsStream.foreach(pos => if(position == pos) return true)
        false
      }

      def wrapperFunc(position: Position, ships: Seq[Ship]): Boolean = ships match {
        case Nil => false
        case _ => if(isPointInShip(position, ships.head)) true else wrapperFunc(position, ships.tail)
      }

      wrapperFunc(position, defenderShips)
    }

    def printBoard() = {
      println("\n" + grid.toString)
    }

    def createPositionsFromShip(ship: Ship): List[Position] = {
      if (ship.orientation == Vertical) List.range(ship.position.y, ship.position.y + ship.length).map(p => Position(ship.position.x, p))
      else List.range(ship.position.x, ship.position.x + ship.length).map(p => Position(p, ship.position.y))
    }

    def addShip(ship: Ship) = {

      def pointsAreInvalid(positions :Seq[Position]): Boolean = {
        positions.exists(p => isHitShip(p, ships)  || !grid.validatePosition(p) || positions.size < 2)
      }

      val points = createPositionsFromShip(ship)
      if(pointsAreInvalid(points)) Left((this, ResponseMessage.boatPlaceErr))
      else Right(createPlayer(name, grid,  ships :+ ship), ResponseMessage.boatPlaced)
    }

    def getAttack(): Position = ???

  }


  case class Human(override val name: String, override val grid: Grid = Gridson.empty, override val ships: Seq[Ship] = Seq.empty[Ship]) extends Player {

    def successfulAttack(p: Position, defender: Player) = (Human(name, grid.newStatus(p, Hit), ships) , defender, ResponseMessage.hit)
    def sunkAttack(positions: List[Position], defender: Player) = (Human(name, grid.addStatus(positions, Sunk), ships) , defender, ResponseMessage.sunk)
    def missedAttack(p: Position, defender: Player) =  (Human(name, grid.newStatus(p, Miss), ships), defender, ResponseMessage.miss)
    def alreadyHitAttack(defender: Player) = (Human(name, grid, ships), defender, ResponseMessage.already_hit)
    def createPlayer(name: String, grid: Grid = Gridson.empty, ships: Seq[Ship] = Seq.empty[Ship]) = Human(name,grid,ships)

    def parseShipFromUserInput(): Try[Ship] = {
      println("place ship: x y length vertical[true/false]")
      val line = readLine
      val t = line.split(" ")
      Try(
        Ship(Position(t(0).toInt,t(1).toInt),t(2).toInt, if (t(3).toBoolean) Vertical else Horizontal)
      )
    }

    def placeShip(u: Player, numShips:Int, grid: Grid = Gridson.empty): Player = {

      def printErrorAndRetry(e: String): Player = {
        println(e)
        placeShip(u, numShips)
      }

      def addShipAndContinue(ship: Ship): Player = {
        u.addShip(ship) match{
          case Left((user, resp)) =>
            println(resp)
            println(grid.toString)
            placeShip(user, numShips, grid)
          case Right((user, resp)) =>
            val newGrid = grid.addShip(createPositionsFromShip(ship))
            println(resp)
            println(newGrid.toString)
            placeShip(user, numShips - 1, newGrid)
        }
      }

      def getShipFromUser: Player = {
        parseShipFromUserInput match {
          case Failure(e) => printErrorAndRetry("Error placing ship, try again")
          case Success(s) => addShipAndContinue(s)
        }
      }

      if(numShips == 0) u
      else getShipFromUser
    }

    def placeShips(): Player = {
      println(s"placing ships for ${this.name}")
      placeShip(this, numberOfUserBoats)
    }

    override def getAttack(): Position = {
      println("attack with coordinates: x y")
      val line = readLine()
      Try{
        val coordinates = line.split(" ").map(_.toInt)
        Position(coordinates.head, coordinates(1))}
      match {
        case Success(pos) if grid.validatePosition(pos)=> pos
        case _ => {
          println("Wrong input. Try again")
          getAttack
        }
      }

    }

  }

  case class Bot(override val name: String, override val grid: Grid = Gridson.empty, override val ships: Seq[Ship] = Seq.empty[Ship]) extends Player  {


    def successfulAttack(p: Position, defender: Player) = (Bot(name, grid.newStatus(p, Hit), ships) , defender, ResponseMessage.hit)
    def sunkAttack(positions: List[Position], defender: Player) = (Bot(name, grid.addStatus(positions, Sunk), ships) , defender, ResponseMessage.sunk)
    def missedAttack(p: Position, defender: Player) =  (Bot(name, grid.newStatus(p, Miss), ships), defender, ResponseMessage.miss)
    def alreadyHitAttack(defender: Player) = (Bot(name, grid, ships), defender, ResponseMessage.already_hit)
    def createPlayer(name: String, grid: Grid = Gridson.empty, ships: Seq[Ship] = Seq.empty[Ship]) = Bot(name,grid,ships)

    override def getAttack(): Position = {

      def findDirection(hitPositions: List[Position]): Option[ShipOrientation] = hitPositions match {
        case hP if hP.size == 1 => None
        case _ if hitPositions.head.x == hitPositions.tail.head.x => Some(Horizontal)
        case _ => Some(Vertical)
      }

      def validateAttackPosition(position: Position) = {
        grid.validatePosition(position) && grid.statusAt(position) == Undiscovered
      }

      def chooseAction(position: Position): Position = {
        val x = position.x
        val y = position.y
        if(validateAttackPosition(Position(x-1,y))) Position(x - 1,y)
        else if(validateAttackPosition(Position(x+1,y))) Position(x + 1,y)
        else if(validateAttackPosition(Position(x,y-1))) Position(x,y-1)
        else Position(x,y+1)
      }

      Thread.sleep(1000) // simulate thinking
      grid.hitPositions match {
        case hPositions if hPositions.size == 1 => chooseAction(hPositions.head)
        case hPositions if hPositions.size > 1 => {
          if(findDirection(hPositions).get == Vertical) {
            if(validateAttackPosition(Position(hPositions.map(_.x).min - 1, hPositions.head.y))) Position(hPositions.map(_.x).min - 1, hPositions.head.y)
            else Position(hPositions.map(_.x).max + 1, hPositions.head.y)
          }
          else {
            if(validateAttackPosition(Position(hPositions.head.x, hPositions.map(_.y).min - 1))) Position(hPositions.head.x, hPositions.map(_.y).min - 1)
            else Position(hPositions.head.x, hPositions.map(_.y).max + 1)
          }
        }
        case _ => Position(Random.nextInt(Constants.gridWidth), Random.nextInt(Constants.gridHeight))
      }
    }

    def placeShip(player: Player, shipsToPlace: Int, lengths: Seq[Int], grid: Grid = Gridson.empty): Player = {

      def createRandomShip(length: Int): Ship = {
        val x = Random.nextInt(Constants.gridWidth)
        val y = Random.nextInt(Constants.gridHeight)
        val orientation = Random.nextInt(2) match {
          case 0 => Vertical
          case _ => Horizontal
        }
        Ship(Position(x,y),length,orientation)
      }

      def addShipAndContinue(ship: Ship): Player = {
        player.addShip(ship) match{
          case Left((user, _)) =>
            placeShip(user, shipsToPlace, lengths, grid)
          case Right((user, _)) =>
            val newGrid = grid.addShip(createPositionsFromShip(ship))
            if(shipsToPlace == 1) println(newGrid.toString) // show how ai placed boats
            placeShip(user, shipsToPlace - 1, lengths.tail, newGrid)

        }
      }

      def createShip(length: Int): Player = {
        addShipAndContinue(createRandomShip(length))
      }

      if(shipsToPlace == 0) {
        player
      }
      else createShip(lengths.head)
    }

    def placeShips(): Player = {
      println(s"Place ships for player -  ${this.name}")
      placeShip(this, shipSizesForAI.size, shipSizesForAI)
    }
  }

}
