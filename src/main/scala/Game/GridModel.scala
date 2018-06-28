package Game


object GridModel {

  import Game.Constants._

  sealed trait CellStatus
  case object Hit extends CellStatus
  case object Miss extends CellStatus
  case object Sunk extends CellStatus
  case object Ship extends CellStatus
  case object Undiscovered extends CellStatus

  type Row = Seq[CellStatus]

  case class Grid(Rows: Seq[Row]) {

    def getRow(i: Int) = Rows(i)

    def rowToString(i: Int): String = {
      Rows(i).map(
        cell => cell match {
            case c if c == Hit => "X"
            case c if c == Miss => "O"
            case c if c == Sunk  => "^"
            case c if c == Ship => "S"
            case _ => "*"
          }).mkString(" ")
    }

    def newStatus(position: Position, status: CellStatus): Grid = {
      Grid(Rows.take(position.x) ++ Seq(getRow(position.x).patch(position.y, Seq(status), 1)) ++ Rows.drop(position.x + 1))
    }

    def addStatus(positions: Seq[Position], status: CellStatus) = {
      positions.foldLeft(this)((p,pos) => p.newStatus(pos, status))
    }

    def addShip(positions: Seq[Position]) = {
      addStatus(positions, Ship)
    }

    def statusAt(position: Position): CellStatus = {
      getRow(position.x)(position.y)
    }

    def hitCounter(): Int = {
        val hits = Rows.flatten.groupBy(identity).mapValues(_.size).get(Hit)
        hits match {
          case Some(hits) => Some(hits).get
          case _ => 0
      }
    }

    def hitAndSinkCounter(): Int = {
      val hits = Rows.flatten.groupBy(identity).mapValues(_.size).get(Hit)
      val sunks = Rows.flatten.groupBy(identity).mapValues(_.size).get(Sunk)
      Pair(hits, sunks) match {
        case Pair(Some(hits), Some(sunks)) => Some(hits + sunks).get
        case (Some(hits), _) => Some(hits).get
        case (_, Some(sunks)) => Some(sunks).get
        case _ => 0
      }
    }

    def hitPositions(): List[Position] = {
      (for {
        x <- 0 until gridWidth
        y <- 0 until gridHeight
        if (statusAt(Position(x,y)) == Hit)
      } yield Position(x,y)).toList
    }

    def validatePosition(position: Position): Boolean = position match {
      case Position(x,y) if x >= 0 && y >= 0 && x < gridWidth && y < gridHeight => true
      case _ => false
    }

    override def toString: String = {

      def wrapItUp(rows: String) = {
        val splitRows = rows.split("\n")
        val rowsWithBorder = List.range(0, gridHeight).zip(splitRows).map{ case(acc ,cell) => s"$acc " + cell}.mkString("\n")
        rowBorder + "\n" + rowsWithBorder
      }

      val rawValues = List.range(0, gridHeight).map(rowToString).mkString("\n")
      wrapItUp(rawValues)

    }
  }
}






