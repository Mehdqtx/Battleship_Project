import org.scalatest.FunSuite

class BoatTest extends FunSuite {

  test("testAddBoatPositions") {

    val pos = (0,0)
    val allPositions = List((1,1),(2,1),(3,1))
    val boat = Boat("Carrier", 5)

    assert(boat.addBoatPositions(pos,allPositions).head == pos)
    assert(boat.addBoatPositions(pos,allPositions).tail == allPositions)
  }

  test("testPlaceBoat") {

    val boat = Boat("Carrier", 5)
    val grid = List.fill(10)(List.fill(10)(0))

    assert(boat.placeBoat(0,0,grid,boat.size,"V")._1 contains (0,0))
    assert(boat.placeBoat(0,0,grid,boat.size,"V")._1 contains (1,0))
    assert(boat.placeBoat(0,0,grid,boat.size,"V")._1 contains (2,0))
    assert(boat.placeBoat(0,0,grid,boat.size,"V")._1 contains (3,0))
    assert(boat.placeBoat(0,0,grid,boat.size,"V")._1 contains (4,0))

    assert(!(boat.placeBoat(0,0,grid,boat.size,"V")._1 contains (5,0)))


    assert(boat.placeBoat(0,0,grid,boat.size,"V")._2(0)(0) == 1)
    assert(boat.placeBoat(0,0,grid,boat.size,"V")._2(1)(0) == 1)
    assert(boat.placeBoat(0,0,grid,boat.size,"V")._2(2)(0) == 1)
    assert(boat.placeBoat(0,0,grid,boat.size,"V")._2(3)(0) == 1)
    assert(boat.placeBoat(0,0,grid,boat.size,"V")._2(4)(0) == 1)

    assert(!(boat.placeBoat(0,0,grid,boat.size,"V")._2(5)(0) == 1))

  }

  test("testIsSunk") {

    val positionList = List((0,0),(0,1),(0,2))
    val emptyPositionList = List()
    assert(!Boat.isSunk(positionList))
    assert(Boat.isSunk(emptyPositionList))
  }

  test("testVerifyAllPosition") {
    val grid = List.fill(10)(List.fill(10)(0))
    assert(Boat.verifyAllPosition(0,0,grid,5,"V"))

    var newGrid = grid.updated(1, grid.apply(1).updated(0,1))
    newGrid = newGrid.updated(2, newGrid.apply(2).updated(0,1))
    newGrid = newGrid.updated(3, newGrid.apply(3).updated(0,1))
    newGrid = newGrid.updated(4, newGrid.apply(4).updated(0,1))
    newGrid = newGrid.updated(5, newGrid.apply(5).updated(0,1))

    assert(!Boat.verifyAllPosition(0,0,newGrid,5,"V"))
    assert(Boat.verifyAllPosition(0,1,newGrid,5,"H"))

  }

}
