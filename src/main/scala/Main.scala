import java.lang.StackWalker.Option
import jdk.jshell.execution.Util

import scala.annotation.tailrec

object Main extends App {




  println("****************************")
  println("***** SCALA BATTLESHIP *****")
  println("****************************")


  Utility.displayMenu()



  /** Initialize a game by creating each player, position their fleet and call the loop "play" to shoot until game is finished.
    *
    * @param level is the type of Player he is playing against. 0 its Human vs Human, 1 its AI Beginner, 2 its AI Medium, 3 its AI Hard
    * @param turn Determine which player start shooting
    */
  def humanVsAI(level:Int, turn: Boolean = true): Unit ={

    if(level == 0 ) println("***** HUMAN VS HUMAN *****")
    else  println("***** HUMAN VS AI  *****")

    //Create human player
    val p1 = Utility.createBasicPlayer("Player1")

    //Create AI or other Human (depend on the level value)
    val p2 = Utility.createBasicPlayer("Player2", level)

    // Positioning the fleet
    println()
    println(p1.name+" start to place his fleet")
    val fleetPlayer1 = p1.positionPlayerFleet(p1.fleet,p1.shotGrid,p1.fleet.getOrElse(List()).size)
    val player1 = p1.copy(_personalGrid = fleetPlayer1._1, _fleet = fleetPlayer1._2)

    println()
    println(p2.name+" start to place his fleet")
    val fleetPlayer2 = p2.positionPlayerFleet(p2.fleet,p2.shotGrid,p2.fleet.getOrElse(List()).size)
    val player2 = p2.copy(_personalGrid = fleetPlayer2._1,_fleet = fleetPlayer2._2)
    if(turn) play(player1,player2)
    else play(player2,player1)


    println("REPLAY ? TYPE Y FOR YES, N FOR NO")
    val choice = scala.io.StdIn.readLine().toUpperCase
    choice match{
      case "Y" => humanVsAI(level, !turn)
      case _ => Utility.displayMenu()
    }
    Utility.displayMenu()
  }


  /** Ask player to shoot a position on enemy's grid. Shot until game is finished.
    *
    * @param starter the player that will shoot a position on enemy's grid
    * @param enemy the player that will receive the shoot on his personal grid
    * @return the player who has won
    */
  def play(starter: Player, enemy: Player): Player={

      // Display grid only when its a human who's playing
      if(starter.typeOfPlayer == 0){
        println(starter.name.toUpperCase()+" TURN TO PLAY")
        println()

        Utility.displayGrid("PERSONAL GRID",starter.personalGrid)
        Utility.displayGrid("SHOT GRID", starter.shotGrid)
      }

      println("==> "+starter.name.toUpperCase()+" <==")
      val coordinates = starter.askCoordinateToShoot()

      val shotX = coordinates._1
      val shotY = coordinates._2

      if(Player.inputShotIsValid(shotX, shotY)){


        val players = starter.shoot(shotX,shotY,enemy)
        val currentPlayer = players._1
        val currentEnemy = players._2

        val win = currentPlayer.verifyWin(currentEnemy)

        if(win){
          println(currentPlayer.name+" win against "+currentEnemy.name)
          println()
          return currentPlayer
        }else{
          play(currentEnemy.copy(),currentPlayer.copy())
        }

      }else{
        println("WRONG COORDINATES,CHOOSE 0=< X,Y <10")
        play(starter.copy(_memoryShotAI = Nil, _memoryDirection = Nil),enemy.copy())
      }

  }

  /**Provide :
    * 100 times AI Level Beginner vs Level Medium
    * 100 times AI Level Beginner vs Level Hard
    * 100 times AI Medium vs Level Hard
    */
  def AIvsAI():Unit= {

    val p1 = Utility.createBasicPlayer("AI level Beginner",1)
    val p2 = Utility.createBasicPlayer("AI level Medium",2)
    val p3 = Utility.createBasicPlayer("AI level Hard",3)



    val game1 = gameLoop(p1,0,p2,0,100, true)
    val game2 = gameLoop(p1,0,p3,0,100,true)
    val game3 = gameLoop(p2,0,p3,0,100,true)

    println(game1._1+" scores "+game1._2+" // "+game1._3+" scores "+game1._4)
    println(game2._1+" scores "+game2._2+" // "+game2._3+" scores "+game2._4)
    println(game3._1+" scores "+game3._2+" // "+game3._3+" scores "+game3._4)

    Utility.writeCsv(List(game1,game2,game3))
    println()
    Utility.displayMenu()
  }


  /**Will initialize and ask players to shoot a position on enemy's grid. Shot until game is finished. Playing 100 games, switching player who start shooting each times.
    *
    * @param player1 First Player
    * @param scorePlayer1 player1 scores. It increase each time player1 win a game.
    * @param player2 Second Player
    * @param scorePlayer2 player2 scores. It increase each time player1 win a game.
    * @param round number of round left before the loop stops.
    * @param turn Boolean that switch starting player. True = player1 will start, False = player2 will start
    * @return A tuple containing le player1 name with his score and the player2 name with his score
    */
  def gameLoop(player1 : Player, scorePlayer1: Int =0,player2: Player, scorePlayer2 : Int, round :Int = 100, turn : Boolean): (String,Int,String,Int) ={
    if(round == 0)( player1.name,scorePlayer1,player2.name,scorePlayer2)
    else{
      val fleetPlayer1 = player1.positionPlayerFleet(player1.fleet,player1.personalGrid,player1.fleet.getOrElse(List()).size)
      val play1 = player1.copy(_personalGrid = fleetPlayer1._1, _fleet = fleetPlayer1._2)


      val fleetPlayer2 = player2.positionPlayerFleet(player2.fleet,player2.personalGrid,player2.fleet.get.size)
      val play2 = player2.copy(_personalGrid = fleetPlayer2._1,_fleet = fleetPlayer2._2)
      if(turn){
        val winner = Main.play(play1,play2)
        if(winner.name == play1.name) gameLoop(player1.copy(), scorePlayer1 +1 ,player2.copy() , scorePlayer2, round-1, !turn)
        else  gameLoop(player1.copy(), scorePlayer1 ,player2.copy(), scorePlayer2 +1, round-1, !turn)
      }else{
        val winner = Main.play(play2,play1)
        if(winner.name == play1.name) gameLoop(player1.copy(), scorePlayer1 +1 ,player2.copy() , scorePlayer2, round-1, !turn)
        else  gameLoop(player1.copy(), scorePlayer1 ,player2.copy(), scorePlayer2 +1, round-1, !turn)
      }
    }
  }



}
