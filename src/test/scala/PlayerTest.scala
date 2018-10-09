import java.lang.StackWalker.Option

import org.scalatest.FunSuite

import scala.Some

class PlayerTest extends FunSuite {

  test("testVerifyWin") {
    val p1 = Player("Player 1",List(List(10)),List(List(10)),Some(List(Boat("Battleship",4))))

    val enemy = Player("enemy",List(List(10)),List(List(10)),Some(List(Boat("Battleship",0))))
    val enemy2 = Player("enemy",List(List(10)),List(List(10)),Some(List(Boat("Battleship",3))))

    assert(p1.verifyWin(enemy) == true)
    assert(p1.verifyWin(enemy2) == false)
  }

  test("testFindPosition") {

    val p1 = Player("Player 1",List.fill(10)(List.fill(10)(0)),List.fill(10)(List.fill(10)(0)),Some(List(Boat("Battleship",4))))
    val boat = Boat("Battleship",4, Some(List((0,0))))
    val boat2 = Boat("Battleship",4, Some(List((0,0),(0,1))))

    assert(p1.findPosition(0,0,boat).size == 0)

    // The new boat positions list have one less element than the enemy boat because one position got hit (remove)
    assert(p1.findPosition(0,0,boat2).positions.get.size == boat2.positions.get.size - 1 )
    assert(p1.findPosition(0,0,boat2).positions.get.contains(0,0) == false )

  }

  test("testShoot") {
    val p1 = Player("Player1",List.fill(10)(List.fill(10)(0)),List.fill(10)(List.fill(10)(0)),Some(List(Boat("Battleship",4,Some(List((0,0),(0,1)))))))
    val p2 = Player("Player2",List.fill(10)(List.fill(10)(0)),List.fill(10)(List.fill(10)(0)),Some(List(Boat("Battleship",4, Some(List((0,0)))))))




  }

  test("testPositionPlayerFleet") {
    val p1 = Player("Player1",List.fill(10)(List.fill(10)(0)),List.fill(10)(List.fill(10)(0)),Some(List(Boat("Battleship",4))))
    val p2 = Player("Player2",List.fill(10)(List.fill(10)(0)),List.fill(10)(List.fill(10)(0)),Some(List(Boat("Battleship",4))))

    //assert(p1.positionPlayerFleet())
  }

  test("testShotGrid") {

  }

  test("testDisplayGrid") {

  }

  test("testAskPlaceBoat") {
    val grid = List(List(0,0,0,0),(0,0,0,0,0))
    val boat = Boat("C", 5)

    //assert(Player.askPlaceBoat(boat, grid))
  }

  test("testIsAlreadyHit") {

  }

  test("testInputIsValid") {

  }

  test("testInputShotIsValid") {

  }

  test("testIsHit") {

  }

}
