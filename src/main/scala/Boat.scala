import scala.annotation.tailrec

/**Boat class containing name, size and a position list
  *
  * @param _name the boat's name
  * @param _size the boat's size
  * @param _positions List of boat position in the grid
  */
case class Boat( _name: String, _size : Int, _positions: Option[List[(Int,Int)]] = None) {

  // Getters
  def name: String = _name
  def size: Int = _size
  def positions: Option[List[(Int,Int)]] = _positions


  /** Place all position of a boat in a grid.
    * @param x row coordinate to place
    * @param y column coordinate to place
    * @param grid the grid to modify after positioning the boat
    * @param size boat size to place
    * @param direction boat direction (Horizontally or vertically)
    * @param boatPositions boat's list of position
    * @return The new boat with his new list of position and the modified grid containing the boat placed
    */
  def placeBoat(x: Int, y:Int, grid: List[List[Int]],size:Int, direction:String, boatPositions : List[(Int,Int)] = Nil): (List[(Int,Int)],List[List[Int]]) ={
    val oldGrid: List[List[Int]] = grid

    // every boat cells has been placed
    if(size == 0) return (boatPositions,grid)
    else{
      try{
        val newGrid = oldGrid.updated(x, oldGrid.apply(x).updated(y,1))
        val allBoatPositions = addBoatPositions((x,y), boatPositions)
        // Player choose to place horizontally his boat
        if(direction == "V"){
          this.placeBoat(x+1,y,newGrid,size-1,direction,allBoatPositions)
        }
        // Player choose to place vertically his boat
        else{
          this.placeBoat(x,y+1,newGrid,size-1,direction,allBoatPositions)
        }
      } catch {
        case e: Exception => this.placeBoat(x,y+1,grid,0,direction,boatPositions)
      }

    }
  }

  /** Add a position in the boat's list of position
    *
    * @param positions the position to add to the list of position
    * @param allPositions the list of position containing all positions
    * @return the new list containing the new position + old ones
    */
  def addBoatPositions(positions: (Int, Int), allPositions: List[(Int, Int)]):List[(Int,Int)] = {
    positions::allPositions
  }


}

object Boat{

  /**Test if a boat is sunk
    *
    * @param newPositions a list of tuples( containing coordinate for each position)
    * @return True if the list of positions is empty (it's sunk), else False.
    */
  def isSunk(newPositions: List[(Int, Int)]): Boolean = if (newPositions.isEmpty) true else false


  /** Will determine if a boat can be placed in a particular direction and coordinate will not overlap an other boat or go out or the grid.
    *
    * @param x the row coordinate to place in the grid
    * @param y the column coordinate to place in the grid
    * @param grid grid containing boat.
    * @param size the boat size to verify if it can be placed here
    * @param direction the direction of the boat to place
    * @return True if the boat (all cells) can be placed here in a particular direction without overlaping or go out of grid. Else false
    */
  @tailrec
  def verifyAllPosition(x:Int ,y:Int, grid: List[List[Int]],size:Int,direction:String): Boolean ={

      if (size == 0)  true
      else{
        if(Player.inputIsValid(x, y,direction)) {
          //Position in the grid is 0 mean its Ocean so there is no ship here
          if (grid(x)(y) == 0) {
            // Player choose to place horizontally his boat
            if (direction == "V") {
              verifyAllPosition(x + 1, y, grid, size - 1, direction)
            }
            // Player choose to place vertically his boat
            else {
              verifyAllPosition(x, y + 1, grid, size - 1, direction)
            }
          }else  false

          // The grid contains a ship in this position
        }else  false

      }
  }

}
