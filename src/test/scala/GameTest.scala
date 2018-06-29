import Game.GameModel.{Bot, Human}
import Game.Ship
import Game.ShipOrientation._
import Game.Position
import org.scalatest._
import Game.Gridson
import Game.ResponseMessage._


class GameTest extends FlatSpec {

  "Player specs" should "be correctly placed" in {
    val player = Human("Pawel")
    assert(player.name === "Pawel", true)
    assert(player.ships.size === 0, true)
    player.addShip(Ship(Position(2,2),2,Horizontal)) match {
      case Right((user,response)) => {
        assert(user.ships.size === 1, true)
        assert(response === boatPlacedCorrectly)
      }
      case _ =>
    }
  }

  "Sinking defender ships methods" should "be working correctly" in {
    val playerAtt = Human("Attacker")
    Human("Defender").addShip(Ship(Position(0,0), 2, Vertical)) match {
      case Right((defender, response)) => {
        assert(playerAtt.checkIfSunk(Position(0,0), defender.ships) === None)
        assert(playerAtt.isHitShip(Position(0,0), defender.ships) === true)
        assert(playerAtt.isHitShip(Position(0,1), defender.ships) === true)
        val (after1stAttack,_,respHit) = playerAtt.attack(Position(0,0), defender)
        assert(respHit === hitAttack)
        assert(after1stAttack.checkIfSunk(Position(0,0), defender.ships) === None)
        val (_,_,respSunk) = after1stAttack.attack(Position(0,1), defender)
        assert(respSunk === sunk )

        // now missed or not enabled attacks
        val (_,_,respMiss) = playerAtt.attack(Position(5,5), defender)
        assert(respMiss === missedAttack)
        val (_,_,respErr) = playerAtt.attack(Position(-5,2), defender)
        assert(respErr === error)
      }
      case _ =>
    }
  }

  "Attack defender ships methods" should "be working correctly" in {
    val playerAtt = Human("Attacker")
    Human("Defender").addShip(Ship(Position(0,0), 2, Vertical)) match {
      case Right((defender, response)) => {
        assert(playerAtt.checkIfSunk(Position(0,0), defender.ships) === None)
        assert(playerAtt.isHitShip(Position(0,0), defender.ships) === true)
        assert(playerAtt.isHitShip(Position(0,1), defender.ships) === true)
        val (after1stAttack,_,respHit) = playerAtt.attack(Position(0,0), defender)
        assert(respHit === hitAttack)
        assert(after1stAttack.checkIfSunk(Position(0,0), defender.ships) === None)
        val (after2ndAttack,_,respSunk) = after1stAttack.attack(Position(0,1), defender)
        assert(respSunk === sunk )

        // now missed or not enabled attacks
        val (_,_,respMiss) = playerAtt.attack(Position(5,5), defender)
        assert(respMiss === missedAttack)
        val (_,_,respErr) = playerAtt.attack(Position(-5,2), defender)
        assert(respErr === error)
      }
      case _ =>
    }
  }

  "Bot methods" should "be working correctly" in {
    val emptyGrid = Gridson.empty
    val botAtt = Bot("Attacker")
    Human("Defender").addShip(Ship(Position(0,0), 2, Vertical)) match {
      case Right((defender, resp)) => {
        assert(resp === boatPlacedCorrectly)
        assert(emptyGrid.validatePosition(botAtt.getAttack) === true)
        val (newBot,_,response) = botAtt.attack(Position(0,0), defender)
        assert(response === hitAttack)

        // bot will for sure sink the boat in at most 2 moves
        val(evenNewerBot,_,someResponse) = newBot.attack(newBot.getAttack, defender)
        assert(someResponse === sunk || someResponse === missedAttack)
        val(newestBot,_,someOtherResponse) = evenNewerBot.attack(evenNewerBot.getAttack, defender)
        assert(someOtherResponse === sunk || someOtherResponse === missedAttack)
        assert(newestBot.grid.hitAndSinkCounter === 2)
      }
      case _ =>
    }
  }
}
