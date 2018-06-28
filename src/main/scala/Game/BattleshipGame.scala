package Game
import Game.GameModel.{Bot, Human, Player}
import scala.io.StdIn.readLine

object BattleshipHelper{

    def initGame(): Unit = {
      println("............................BattleshipGame.............................\n")
      Thread.sleep(1000)
      println("Show your courage and strategy and win with the Enemy - the almighty AI\n")
      Thread.sleep(1000)
    }

    def giveMeName(): String = {
      println("Enter your name: ")
      readLine
    }

    def startGame(humanName: String, botName: String) = {

      def playRound(attacker: Player, defender: Player):Unit = {
        println(s"It's ${attacker.name}'s turn!")
        attacker.grid.toString
        val currentAttack = attacker.getAttack
        val (currentAttacker, currentDefender, status) = attacker.attack(currentAttack, defender)
        println("That's a .... " + status)
        println(currentAttacker.grid.toString)
        currentAttacker.grid.hitAndSinkCounter match {
          case cnt if cnt > 0 && (cnt == currentDefender.ships.map(_.length).sum) => println(s"${attacker.name} is a winner!")
          case _ if (status != "miss") => playRound(currentAttacker,currentDefender)
          case _ => playRound(currentDefender,currentAttacker)
        }
      }

      val human = Human(humanName).placeShips()
      println
      val bot = Bot(botName).placeShips()
      println
      println("Lets fight!")
      playRound(human, bot)
    }

}

object BattleshipGame {

  import BattleshipHelper._

  def main(String: Array[String]) = {
    initGame
    val playerName1 = giveMeName
    val playerName2 = giveMeName
    startGame(playerName1, playerName2)
  }

}
