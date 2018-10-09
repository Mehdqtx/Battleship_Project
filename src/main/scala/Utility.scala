import java.io.File
import com.github.tototoshi.csv.CSVWriter


import scala.Console._

import scala.util.Random




object Utility {


  /**
    * Display main menu
    */
  def displayMenu(){
    println(" Choose a game mode : ")
    println(" Enter 1 for HUMAN VS HUMAN MODE")
    println(" Enter 2 for HUMAN VS AI MODE")
    println(" Enter 3 for AI VS AI (csv result)")
    println(" Enter 4 to quit the game")
    scala.io.StdIn.readLine() match {
      case "1"  => Main.humanVsAI(0)
      case "2"  => chooseAiLevel()
      case "3"  => Main.AIvsAI()
      case "4"  => System.exit(0)
      case _  => displayMenu()
    }
  }

  /**
    * Display AI choices
    */
  def chooseAiLevel(){
    println()
    println(" Choose AI's level : ")
    println(" Enter 1 for AI Low")
    println(" Enter 2 for AI Medium")
    println(" Enter 3 for AI Hard")
    println(" Enter 4 return to the menu")

    scala.io.StdIn.readLine() match {
      case "1"  => Main.humanVsAI(1)
      case "2"  => Main.humanVsAI(2)
      case "3"  => Main.humanVsAI(3)
      case _  => displayMenu()
    }
  }

  /** Create a basic player with a initial grid and list of boats
    *
    * @param name Player's name
    * @param level Determine if its an AI or not. 0 for Human, 1 for AI Beginner, 2 for AI Medium, 3 for AI Hard
    * @return A new Player
    */
  def createBasicPlayer(name:String, level:Int = 0): Player = {
    val BOATS = List(
      Boat("Carrier",5),
      Boat("Battleship",4),
      Boat("Cruiser",3),
      Boat("Submarine",3),
      Boat("Destroyer",2)
    )
    Player(name, List.fill(10)(List.fill(10)(0)),List.fill(10)(List.fill(10)(0)),Some(BOATS),level)
  }

  /**Ask user for coordinate (Row, Line) to place his boat
    *
    * @param typeOfPlayer the type of player (AI Or Human) to determine if its necessary to place his fleet automatically
    * @return a tuple containing row and column coordinates to shoot
    */
  def askCoordinate(typeOfPlayer : Int = 0) : (Int, Int) = {
    if (typeOfPlayer == 0) {
      print("==> Enter row position :  ")
      val inputX = scala.io.StdIn.readLine()

      print("==> Enter column position :  ")
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
      val inputX = (new Random).nextInt(10)
      val inputY = (new Random).nextInt(10)
      (inputX,inputY)
    }
  }


  /** Ask user to enter a direction. Automatically if its an AI
    *
    * @param typeOfPlayer the type of player who needs to enter direction. 0 = Human, 1/2/3 = AI
    * @return the direction as a string.
    */
  def askDirection(typeOfPlayer : Int = 0) : String = {

    if(typeOfPlayer == 0){

      val direction: String = scala.io.StdIn.readLine().toUpperCase
      return direction
    }
    val rnd = (new Random).nextInt(2) % 2
    if(rnd == 0) "V"
    else "H"
  }

  /** Display players grid.
    *
    * @param msg the message to display just in front of the grid
    * @param grid the grid to display
    */
  def displayGrid(msg:String, grid : List[List[Int]]):Unit = {

    println(msg)
    println("   0  1  2  3  4  5  6  7  8  9 ")
    grid.zipWithIndex.foreach{ case(list,i)=> displayLine(list,i)}
    println()
    println()

    /** Display each element of a list.
      *
      * @param line The list to display each element
      */
    def displayLine( line : List[Int], i :Int): Unit={
      print(i+"|")
      line.foreach {
        case e@1 => print(s"${RESET}${GREEN_B}${BLACK} S ${RESET}")
        case e@0 => print(s"${RESET}${BLUE_B}${BLACK} O ${RESET}")
        case e@(-1) => print(s"${RESET}${RED_B}${BLACK} X ${RESET}")


      }
      println()
    }
  }

  /** Write in a csv the result of game between AIs
    *
    * @param games A list of game containing the result of 100 game between two AI
    */
  def writeCsv(games: List[(String,Int,String,Int)]): Unit = {
    val file = new File("./ai_proof.csv")
    val writer = CSVWriter.open(file)
    writer.writeRow(List("AI Name", "score", "AI Name 2", "score 2"))
    games.foreach(game => {
      writer.writeRow(List(game._1, game._2, game._3, game._4))
    })
    writer.close()
  }

}