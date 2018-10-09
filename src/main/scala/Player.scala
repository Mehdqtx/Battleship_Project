import Utility.askCoordinate

import scala.annotation.tailrec
import scala.util.Random



/** A player playing the battleship game.
  *
  * @param _name the player's name
  * @param _personalGrid the player's personal grid where he will have his boats
  * @param _shotGrid the player's shot grid where he will mark his shots
  * @param _fleet the players's fleet that is a list of not shot boats.
  * @param _typeOfPlayer the player's type, 0 = human, 1 = AI Low, 2 = AI Medium, 3 = AI Hard
  * @param _memoryShotAI a list of memorized shot (only used by AI level Hard). Used to add position of a hit boat and determine his other positions
  * @param _memoryDirection a list of memorized direction (only used by AI level Hard) to remember the direction already visited from a point
  */
case class Player(_name:String, _personalGrid :  List[List[Int]], _shotGrid : List[List[Int]], _fleet: Option[List[Boat]]= None, _typeOfPlayer:Int = 0, _memoryShotAI : List[(Int,Int)] = Nil, _memoryDirection : List[String] = Nil){


  // Getters
  def name: String = _name
  def personalGrid: List[List[Int]] = _personalGrid
  def shotGrid: List[List[Int]] = _shotGrid
  def fleet: Option[List[Boat]] = _fleet
  def typeOfPlayer: Int = _typeOfPlayer
  def memoryShotAI: List[(Int,Int)] = _memoryShotAI
  def memoryDirection: List[String] = _memoryDirection



  /** Verify if a player won the game by verifying entire enemy fleet is sunk
    * @param enemy the enemy player to verify that entire fleet is sunk
    * @return Boolean, True if enemy fleet is sunk, else false
    */
  def verifyWin(enemy: Player):Boolean = {
    val enemyFleet = enemy.fleet.getOrElse(List())

    //retrieve list of sunk boat
    val sunkBoats = enemyFleet.filter(boat => boat.size == 0 )

    // Compare number of ship sank and size of players fleet
    if(sunkBoats.size == enemy.fleet.getOrElse(List()).size) true else false
  }

  /**Removes the position x,y (if they exist in the list) from the position list of the boat entered as parameter
    *
    * @param x row coordinate
    * @param y column coordinate
    * @param boat the boat to remove the position from
    * @return return a new boat with a new list of position
    */
  def findPosition(x: Int, y:Int, boat: Boat): Boat = {
    val boatPositions = boat.positions

    val  newPositions = boatPositions.get.filterNot(p => p == (x,y))

    if(Boat.isSunk(newPositions)) {
      println(boat.name+" IS SUNK !")
      boat.copy(_size = 0)
    }
    else boat.copy(_positions = Some(newPositions))

  }


  /** Shoot a position in the enemy grid
    *
    * @param x row coordinate to shoot
    * @param y column coordinate to shoot
    * @param enemy the enemy player to shoot
    * @return Tuples with two players : First one is the player currently playing with new grids. Second player is the enemy with a new grid a maybe a modified list of boat
    */
  def shoot(x: Int, y: Int, enemy: Player): (Player,Player) = {



      val shotGrid = this.shotGrid

      val enemyGrid = enemy.personalGrid

      if(Player.isHit(x, y, enemyGrid)){

        println("HIT")

        val newShotGrid = shotGrid.updated(x, shotGrid.apply(x).updated(y,1))
        val newPersoGridEnemy = enemyGrid.updated(x, enemyGrid.apply(x).updated(y,-1))

        val enemyFleet = enemy.fleet
        val newEnemyFleet = enemyFleet.get.map(b => findPosition(x, y, b))

        if(this.typeOfPlayer == 3){
          val newMemoryShotAI = (x,y)::this.memoryShotAI
          val newMemoryDirection = verifyMemoryDirection(x,y, this.memoryDirection)
          return (this.copy(_shotGrid=newShotGrid,_memoryShotAI=newMemoryShotAI,_memoryDirection = newMemoryDirection),enemy.copy(_personalGrid=newPersoGridEnemy,_fleet=Some(newEnemyFleet)))
        }

        return (this.copy(_shotGrid=newShotGrid),enemy.copy(_personalGrid=newPersoGridEnemy,_fleet=Some(newEnemyFleet)))
      }
      else{

        println("MISS")
        if(this.memoryShotAI.isEmpty){

          if(Player.isAlreadyHit(x,y,enemyGrid)){
            return (this.copy(),enemy.copy())

          }else{

            val newShotGrid = shotGrid.updated(x, shotGrid.apply(x).updated(y,-1))
            return (this.copy(_shotGrid=newShotGrid),enemy.copy())
          }
        }else{
          val direction = deductDirection(x,y,this.memoryShotAI.head)
          val memoryDirection = verifyMemoryDirection(x,y, this.memoryDirection)
          val newMemoryDirection = direction::memoryDirection
          if((newMemoryDirection contains "bottom") && (newMemoryDirection contains "top") && (newMemoryDirection contains "left") && (newMemoryDirection contains "right")){
            if(Player.isAlreadyHit(x,y,enemyGrid)){
              return (this.copy(_memoryShotAI= Nil, _memoryDirection = Nil),enemy.copy())

            }else{

              val newShotGrid = shotGrid.updated(x, shotGrid.apply(x).updated(y,-1))
              return (this.copy(_shotGrid=newShotGrid,_memoryShotAI= Nil, _memoryDirection = Nil),enemy.copy())
            }
          }else{
            if(Player.isAlreadyHit(x,y,enemyGrid)){
             return (this.copy(_memoryShotAI = List(this.memoryShotAI.last),_memoryDirection = newMemoryDirection),enemy.copy())

            }else{

              val newShotGrid = shotGrid.updated(x, shotGrid.apply(x).updated(y,-1))
              return (this.copy(_shotGrid=newShotGrid,_memoryShotAI = List(this.memoryShotAI.last), _memoryDirection = newMemoryDirection),enemy.copy())
            }
          }
        }


      }


  }

  /** Take the memorized direction and the current coordinate to determine if its possible or not to shot left bottom right or top
    *
    * @param x row index coordinate
    * @param y column index coordinate
    * @param oldDirections the list of direction already visited
    * @return return a new list of direction already visited
    */
  def verifyMemoryDirection(x: Int, y: Int, oldDirections : List[String]): List[String]={

    if(!Player.inputShotIsValid(x+1,y) && !(oldDirections contains "bottom")) verifyMemoryDirection(x,y,"bottom"::oldDirections)
    else if(!Player.inputShotIsValid(x-1,y) && !(oldDirections contains "top")) verifyMemoryDirection(x,y,"top"::oldDirections)
    else if(!Player.inputShotIsValid(x,y-1) && !(oldDirections contains "left")) verifyMemoryDirection(x,y,"left"::oldDirections)
    else if(!Player.inputShotIsValid(x,y+1) && !(oldDirections contains "right")) verifyMemoryDirection(x,y,"right"::oldDirections)
    else oldDirections
  }

  /** Deduct if you shoot to the top, the left, the right or the bottom of a position
    *
    * @param x the shot row
    * @param y the shot column
    * @param shotHit the position to compare with
    * @return Where x,y were located from shotHit
    */
  def deductDirection(x:Int,y:Int, shotHit : (Int,Int)):String = {
    if(x == shotHit._1){
      if (y == shotHit._2 - 1) "left"
      else "right"
    }else{
      if(x == shotHit._1 - 1) "top"
      else "bottom"
    }
  }


  /** Ask the user to enter row and column coordinate to shoot
    *
    * @return row and column coordinate of the shoot
    */
  def askCoordinateToShoot(): (Int, Int) = {
    if (this.typeOfPlayer == 0) {
      print("==> Choose a row coordinate to shoot : ")
      val inputX = scala.io.StdIn.readLine()

      print("==> Choose a column coordinate to shoot : ")
      val inputY = scala.io.StdIn.readLine()


      try {
        val x = inputX.toInt
        val y = inputY.toInt

        (x, y)
      } catch {
        case e: Exception =>
          println()
          println("ERROR, YOU CAN ONLY ENTER A NUMBER BETWEEN 0 AND 9")
          println()

          askCoordinate()
      }
    }else{
      val shootCoordinates = this.shootFromAI()
      val x = shootCoordinates._1
      val y = shootCoordinates._2
      (x,y)
    }
  }


  /** shoot realized automatically for AI. The strength of a shoot depends of AI level
    *
    * @return a tuple containing row and column coordinate
    */
  def shootFromAI(): (Int,Int) ={
    if (this.typeOfPlayer == 1){
      val coordinates = Player.randomShoot()
      return (coordinates._1,coordinates._2)
    }
    else if(this.typeOfPlayer == 2){
        val coordinates = Player.randomShoot()
        if(hasNotBeenAlreadyTried(coordinates._1,coordinates._2))return(coordinates._1,coordinates._2)
        else return this.shootFromAI()
    }
    else{
      val shotMemory = this._memoryShotAI
      if(shotMemory.isEmpty){
        val coordinates = Player.randomShoot()
        if(hasNotBeenAlreadyTried(coordinates._1,coordinates._2)) return (coordinates._1,coordinates._2)
        else return this.shootFromAI()
      }
      else{
        val shotMemory = this._memoryShotAI.head

        val direction = directionNotTested(this.memoryDirection)

        if((direction contains "top") && (direction contains "bottom") && (direction contains "left") && (direction contains "right")){
          val coordinates = Player.randomShoot()
          val x = coordinates._1
          val y = coordinates._2
          if(hasNotBeenAlreadyTried(x,y))return(x,y)
          else return this.shootFromAI()
        }else{
          if(direction == "bottom") return(shotMemory._1+1,shotMemory._2)
          else if(direction == "top") return(shotMemory._1-1,shotMemory._2)
          else if(direction == "left")return (shotMemory._1,shotMemory._2-1)
          else return (shotMemory._1,shotMemory._2+1)
        }
      }

    }
  }


  /** Determine which direction has not already been visited
    *
    * @param memoryDirection containing direction visited
    * @return a direction that has not been visited.
    */
  def directionNotTested(memoryDirection : List[String]):String = {
    if(memoryDirection.isEmpty) return "top"
    else if (!(memoryDirection contains "top"))return "top"
    else if (!(memoryDirection contains "bottom"))return "bottom"
    else if(!(memoryDirection contains "right"))return "right"
    else if(!(memoryDirection contains "left"))return "left"
    else return "full"
  }

  /** Verify if a row+column coordinate were already tried.
    *
    * @param x row coordinate
    * @param y column coordinate
    * @return Boolean. True if it wasn't tried. Else False
    */
  def hasNotBeenAlreadyTried(x : Int,y : Int): Boolean ={
    val grid = this.shotGrid

    if (grid(x)(y) == 0) true
    else false

  }


  /*** Place each boat in the grid.
    *
    * @param starterFleet the fleet to place
    * @param grid to grid to modify while positioning boats
    * @param nbToPlace the amount of boat left to place
    * @param placedFleet the amount of boat placed
    * @return return the new grid containing placed boats and a List of boat which contains, for each boats, all his positions.
    */
  def positionPlayerFleet(starterFleet : Option[List[Boat]], grid : List[List[Int]],nbToPlace: Int, placedFleet : List[Boat] = Nil): (List[List[Int]],Option[List[Boat]]) ={

        // No more boats to place
        if (nbToPlace == 0) (grid,Some(placedFleet))
        else{

          // Take the first boat of the initial list of boats
          val boatToPlace = starterFleet.get.head
          val newBoatAndGrid = Player.askPlaceBoat(boatToPlace,grid, this.typeOfPlayer)
          val boatPlaced = newBoatAndGrid._1
          val newGrid = newBoatAndGrid._2
          if(this.typeOfPlayer == 0)Utility.displayGrid("PERSONAL GRID",newGrid)

          positionPlayerFleet(Some(starterFleet.get.tail),newGrid,nbToPlace-1,boatPlaced::placedFleet)
    }
  }
}

object Player {


  /** Determine if a cell in the grid was already hit
    *
    * @param x  row coordinate
    * @param y column coordinate
    * @param enemyGrid the enemy grid to verify if it was already hit or not
    * @return True if it was already hit, else False
    */
  def isAlreadyHit(x: Int, y: Int, enemyGrid: List[List[Int]]): Boolean = {
    if(enemyGrid(x)(y) == -1) true else false
  }


  /** Determine if a cell is hit
    *
    * @param x row coordinate
    * @param y column coordinate
    * @param enemyGrid the enemy grid to verify if it is hit or not
    * @return True is it's a hit, else false
    */
  def isHit(x: Int, y: Int, enemyGrid: List[List[Int]]): Boolean = {
    try{
      if(enemyGrid(x)(y) == 1) true else false
    }catch{
      case _:Exception => false
    }

  }

  /** Verify if inputs are correct and contained in grid and direction are correct.
    *
    * @param x row coordinate
    * @param y column coordinate
    * @param direction string meaning the direction chosen
    * @return true if its valid, else false
    */
  def inputIsValid(x:Int, y:Int, direction: String): Boolean ={
    if(x<10 && x>=0 && y>=0 && y<10 && (direction == "H" || direction == "V")) true else false
  }

  /** Verify if input to shot are contained in the grid.
    *
    * @param x row coordinate
    * @param y column coordinate
    * @return true if its valid, else false
    */
  def inputShotIsValid(x:Int, y:Int): Boolean ={
    if(x<10 && x>=0 && y>=0 && y<10) true else false
  }


  /**Ask player to place a boat
    *
    * @param boat the boat to place
    * @param grid the grid to modify after positioning the boat.
    * @param typeOfPLayer type of player. 0 if Human, 1/2/3 for AI
    * @return The placed boat (with his new list positions) and the new grid containing the placed boat.
    */
  def askPlaceBoat(boat : Boat, grid: List[List[Int]], typeOfPLayer :Int = 0): (Boat, List[List[Int]]) = {

    println("Positioning " +boat.name+" with size "+boat.size)

    val coordinates = Utility.askCoordinate(typeOfPLayer)

    val inputX = coordinates._1
    val inputY = coordinates._2

    print("Direction : H for horizontally, V for vertically :  ")
    val direction = Utility.askDirection(typeOfPLayer)

    // Verify if inputs are in the grid
    if(Player.inputIsValid(inputX,inputY,direction)){
      if(Boat.verifyAllPosition(inputX,inputY, grid,boat.size,direction)){
        val newBoatPositions = boat.placeBoat(inputX, inputY, grid,boat.size,direction)
        val newBoat = boat.copy(boat.name,boat.size,Option(newBoatPositions._1))
        val newGrid = newBoatPositions._2
        println()
        println(boat.name+" is placed")
        return (newBoat,newGrid)
      }else{
        println("You can't place your ship here, PLEASE RETRY")
        return Player.askPlaceBoat(boat,grid,typeOfPLayer)
      }
    }
    println("Invalid coordinates")
    Player.askPlaceBoat(boat,grid)
  }

  /** Choose random row and column coordinate.
    *
    * @return a tuple containing row coordinate and column coordinate.
    */
  def randomShoot():(Int,Int)= {
    val inputX = (new Random).nextInt(10)
    val inputY = (new Random).nextInt(10)
    (inputX,inputY)
  }




}
