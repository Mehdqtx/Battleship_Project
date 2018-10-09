import org.scalatest.FunSuite

class PlayerTest extends FunSuite {

  test("testVerifyWin") {
    val p1 = Player("Player 1",List(List(10)),List(List(10)),Some(List(Boat("Battleship",4))))

    val enemy = Player("enemy",List(List(10)),List(List(10)),Some(List(Boat("Battleship",0))))
    val enemy2 = Player("enemy",List(List(10)),List(List(10)),Some(List(Boat("Battleship",3))))

    assert(p1.verifyWin(enemy))
    assert(!p1.verifyWin(enemy2))
  }

  test("testFindPosition") {

    val p1 = Player("Player 1",List.fill(10)(List.fill(10)(0)),List.fill(10)(List.fill(10)(0)),Some(List(Boat("Battleship",4))))
    val boat = Boat("Battleship",4, Some(List((0,0))))
    val boat2 = Boat("Battleship",4, Some(List((0,0),(0,1))))

    assert(p1.findPosition(0,0,boat).size == 0)

    // The new boat positions list have one less element than the enemy boat because one position got hit (remove)
    assert(p1.findPosition(0,0,boat2).positions.get.size == boat2.positions.get.size - 1 )
    assert(!p1.findPosition(0, 0, boat2).positions.get.contains(0, 0) )

  }

  test("testShoot") {
    val p1 = Player("Player1",List.fill(10)(List.fill(10)(0)),List.fill(10)(List.fill(10)(0)),Some(List(Boat("Battleship",4,Some(List())))))
    val p2 = Player("Player2",List.fill(10)(List.fill(10)(1)),List.fill(10)(List.fill(10)(0)),Some(List(Boat("Battleship",4, Some(List())))))

    assert(p1.shoot(0,0,p2)._1.shotGrid(0)(0) == 1 )
    assert(p1.shoot(0,0,p2)._2.personalGrid(0)(0) == -1 )

    assert(p2.shoot(5,5,p1)._1.shotGrid(5)(5) == -1 )
    assert(p2.shoot(5,5,p1)._2.personalGrid(5)(5) == 0 )

  }


  test("testDeductDirection") {
    val p1 = Player("Player1",List.fill(10)(List.fill(10)(0)),List.fill(10)(List.fill(10)(0)),Some(List(Boat("Battleship",4,Some(List())))))

    assert(p1.deductDirection(0,1,(1,1)) == "top")
    assert(p1.deductDirection(2,1,(1,1)) == "bottom")
    assert(p1.deductDirection(1,0,(1,1)) == "left")
    assert(p1.deductDirection(1,2,(1,1)) == "right")
  }

  test("testHasNotBeenAlreadyTried") {
    val p1 = Player("Player1",List.fill(10)(List.fill(10)(0)),List(List(0,0,0,0,0),List(0,0,0,0,0)),Some(List(Boat("Battleship",4,Some(List())))))
    val p2 = Player("Player1",List.fill(10)(List.fill(10)(0)),List(List(0,0,0,0,0),List(0,0,0,0,0)),Some(List(Boat("Battleship",4,Some(List())))))

    assert(p1.hasNotBeenAlreadyTried(0,0))
    val newp1 = p1.shoot(0,0,p2)._1

    assert(!newp1.hasNotBeenAlreadyTried(0,0))
  }


  test("testVerifyMemoryDirection") {
    val p1 = Player("Player1",List.fill(10)(List.fill(10)(0)),List(List(0,0,0,0,0),List(0,0,0,0,0)),Some(List(Boat("Battleship",4,Some(List())))))
    val memoryDirection = List()

    assert(p1.verifyMemoryDirection(0,0,memoryDirection) contains "top")
    assert(p1.verifyMemoryDirection(9,9,memoryDirection) contains "bottom")
    assert(p1.verifyMemoryDirection(9,9,memoryDirection) contains "right")
  }


  test("testIsAlreadyHit") {
    val enemyGrid = List(List(-1,-1,0,0,0),List(0,0,0,0,0),List(0,0,0,0,0))
    assert(Player.isAlreadyHit(0, 0, enemyGrid))
    assert(!Player.isAlreadyHit(1, 0, enemyGrid))

  }

  test("testInputIsValid") {
    assert(Player.inputIsValid(0,0,"H"))
    assert(!Player.inputIsValid(15,0,"h"))
    assert(!Player.inputIsValid(0,10,"h"))
    assert(!Player.inputIsValid(2,0,"aea"))
    assert(!Player.inputIsValid(-2,0,"V"))
  }

  test("testInputShotIsValid") {
    assert(Player.inputShotIsValid(0,0))
    assert(!Player.inputShotIsValid(15,0))
    assert(!Player.inputShotIsValid(0,10))
    assert(Player.inputShotIsValid(2,0))
    assert(Player.inputShotIsValid(9,9))
    assert(!Player.inputShotIsValid(-2,0))
  }

  test("testIsHit") {
    val enemyGrid = List(List(1,1,0,0,0),List(0,1,0,0,0),List(0,0,0,0,0))

    assert(Player.isHit(0,0,enemyGrid))
    assert(!Player.isHit(12,0,enemyGrid))
    assert(!Player.isHit(0,15,enemyGrid))
    assert(Player.isHit(0,1,enemyGrid))
    assert(!Player.isHit(0,2,enemyGrid))
    assert(Player.isHit(1,1,enemyGrid))

  }

}
