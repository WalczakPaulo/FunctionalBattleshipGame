package Game
import Game.GameModel.{Bot, Human, Player}
import scala.io.StdIn.readLine
import Game.ResponseMessage.missedAttack
object BattleshipGameHelper{

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
        val currAttack = attacker.getAttack
        val (currAttacker, currDefender, responseMessage) = attacker.attack(currAttack, defender)
        println("That's a .... " + responseMessage)
        println(currAttacker.grid.toString)
        currAttacker.grid.hitAndSinkCounter match {
          case cnt if cnt > 0 && (cnt == currDefender.ships.map(_.length).sum) => println(s"${attacker.name} is a winner!")
          case _ if (responseMessage != missedAttack) => playRound(currAttacker,currDefender)
          case _ => playRound(currDefender,currAttacker)
        }
      }

      val human = Human(humanName).addShips()
      val bot = Bot(botName).addShips()
      println("Lets fight!")
      playRound(human, bot)
    }

}

object BattleshipGame {

  import BattleshipGameHelper._

  def main(String: Array[String]) = {
    initGame
    val playerName1 = giveMeName
    val playerName2 = giveMeName
    startGame(playerName1, playerName2)
  }

}
