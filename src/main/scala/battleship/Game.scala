package battleship

import scala.annotation.tailrec
import scala.util.Random
import scala.io.StdIn.readLine
import scala.util.Try

import BattleShipUtil._
/**
  *
  * @param player
  * @param opponent
  * @param beginner
  * @param mode
  */

case class GameState(player: Player, opponent: Player, beginner: Player, mode: Int, randomDir: Random, randomX: Random, randomY: Random)


/**
  *
  */
object Battleship extends App {
    //val numbers = Array.ofDim[Int](10, 10)

    val boatSize = Array(2, 3, 3, 4, 5)
    val boatName = Array("Destroyer", "Submarine", "Cruiser", "Battleship", "Carrier")

    val randomDir = Random
    val randomX = Random
    val randomY = Random

    selectMode()

    /**
      *
      * @param gameState
      * @param randomX
      * @param randomY
      * @param randomDir
      */
    @tailrec
    def mainLoop(gameState: GameState) {

        showQuestion("Turn of player " + gameState.player.name)

        //If the player is not an AI
        if (gameState.player.isHuman) {
            showQuestion(gameState.player.name + "ships grid")
            affichage(gameState.player)

            showQuestion(gameState.player.name + "shoots grid")
            showMyShoots(gameState.player)

            showQuestion("Enter x position to shoot")
            val shootx = getUserIntInput()

            showQuestion("Enter y position to shoot")
            val shooty = getUserIntInput()

            if (shootx.isEmpty || shooty.isEmpty) {
                mainLoop(gameState)
            } else {
                val shootPosition = Position(shootx.get, shooty.get, false)

                if (shootPosition.isInGrid && gameState.opponent.stillInGame) {
                    val indexShip = gameState.opponent.myBoard.isItTouched(shootPosition).get
                    if (indexShip != -1) {
                        showQuestion("Ship touched in (" + shootPosition.axisX + "," + shootPosition.axisY + ")")
                        val newShootPosition = shootPosition.copy(isTouched = true)
                        val newPlayer = gameState.player.makeAShoot(newShootPosition)
                        val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)

                        if (newPlayer1.myBoard.shipList(indexShip).isSunk) {
                            showQuestion("You sunk " + newPlayer1.name + " " + newPlayer1.myBoard.shipList(indexShip).name)
                        }

                        if (newPlayer1.stillInGame) {
                            val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer)
                            mainLoop(newGameState)
                        } else {
                            val winner = gameState.player.copy(score = newPlayer.score+1 )
                            val newGS = gameState.copy(player = winner)
                            showQuestion(newGS.player.name + " Win")
                            showQuestion(newGS.player.name+" ; "+ newGS.player.score +" ; "+newGS.opponent.name+" ; "+ newGS.opponent.score)
                            choice(newGS)
                        }
                    } else {
                        showQuestion("Miss")
                        val newPlayer = gameState.player.makeAShoot(shootPosition)
                        val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)
                        if (gameState.opponent.stillInGame) {
                            val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer)
                            mainLoop(newGameState)
                        } else {
                            val winner = gameState.player.copy(score = newPlayer.score+1 )
                            val newGS = gameState.copy(player = winner)
                            showQuestion(newGS.player.name + " Win")
                            showQuestion(newGS.player.name+" ; "+ newGS.player.score +" ; "+newGS.opponent.name+" ; "+ newGS.opponent.score)
                            choice(newGS)

                        }
                    }


                }
                else if (!shootPosition.isInGrid) {
                    showQuestion("Please enter coordinate between 0 and 9")
                    mainLoop(gameState)
                } else if (!gameState.opponent.stillInGame) {
                    val winner = gameState.player.copy(score = gameState.player.score+1 )
                    val newGS = gameState.copy(player = winner)
                    showQuestion(newGS.player.name + " Win")
                    showQuestion(newGS.player.name+" ; "+ newGS.player.score +" ; "+newGS.opponent.name+" ; "+ newGS.opponent.score)
                    choice(newGS)
                }
            }
            //If the player is an AI
        } else {
            showQuestion("Ship AI grid")
            affichage(gameState.player)

            showQuestion("AI shoots grid")
            showMyShoots(gameState.player)


            val shootPosition = gameState.player.getShootFromPlayer(gameState.randomX, gameState.randomY).get //Get the shoot from the AI
            if (shootPosition.isInGrid && gameState.opponent.stillInGame) {
                val indexShip = gameState.opponent.myBoard.isItTouched(shootPosition).get
                if (indexShip != -1) {
                    showQuestion("Ship touched in (" + shootPosition.axisX + "," + shootPosition.axisY + ")")
                    val newShootPosition = shootPosition.copy(isTouched = true)
                    val newPlayer = gameState.player.makeAShoot(newShootPosition)
                    val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)

                    if (newPlayer1.myBoard.shipList(indexShip).isSunk) {
                        showQuestion("You sunk " + newPlayer1.name + " " + newPlayer1.myBoard.shipList(indexShip).name)
                    }

                    if (newPlayer1.stillInGame) {
                        val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer)
                        mainLoop(newGameState)
                    } else {
                        val winner = gameState.player.copy(score = gameState.player.score+1 )
                        val newGS = gameState.copy(player = winner)
                        showQuestion(newGS.player.name + " Win")
                        showQuestion(newGS.player.name+" ; "+ newGS.player.score +" ; "+newGS.opponent.name+" ; "+ newGS.opponent.score)
                        choice(newGS)
                    }
                } else {
                    showQuestion("Miss")
                    val newPlayer = gameState.player.makeAShoot(shootPosition)
                    val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)
                    if (gameState.opponent.stillInGame) {
                        showQuestion("My shoots")

                        val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer)
                        mainLoop(newGameState)
                    } else {
                        val winner = gameState.player.copy(score = gameState.player.score+1 )
                        val newGS = gameState.copy(player = winner)
                        showQuestion(newGS.player.name + " Win")
                        showQuestion(newGS.player.name+" ; "+ newGS.player.score +" ; "+newGS.opponent.name+" ; "+ newGS.opponent.score)
                        choice(newGS)

                    }
                }
            }
            else if (!shootPosition.isInGrid) {
                mainLoop(gameState)
            } else if (!gameState.opponent.stillInGame) {
                val winner = gameState.player.copy(score = gameState.player.score+1 )
                val newGS = gameState.copy(player = winner)
                showQuestion(newGS.player.name + " Win")
                showQuestion("Score "+newGS.player.name+" : "+ newGS.player.score)
                showQuestion("Score "+newGS.opponent.name+" : "+ newGS.opponent.score)
                choice(newGS)

            }

        }

    }

    /**
      *
      * @param gameState
      * @param randomX
      * @param randomY
      * @param randomDir
      */
    @tailrec
    def mainLoopAIVSAI(gameState: GameState) : GameState = {
            showQuestion("Ship AI grid")
            affichage(gameState.player)

            showQuestion("My shoots grid")
            showMyShoots(gameState.player)


            val shootPosition = gameState.player.getShootFromPlayer(gameState.randomX, gameState.randomY).get //Get the shoot from the AI
            if (shootPosition.isInGrid && gameState.opponent.stillInGame) {
                val indexShip = gameState.opponent.myBoard.isItTouched(shootPosition).get
                if (indexShip != -1) {
                    showQuestion("Ship touched in (" + shootPosition.axisX + "," + shootPosition.axisY + ")")
                    val newShootPosition = shootPosition.copy(isTouched = true)
                    val newPlayer = gameState.player.makeAShoot(newShootPosition)
                    val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)

                    if (newPlayer1.myBoard.shipList(indexShip).isSunk) {
                        showQuestion("You sunk " + newPlayer1.name + " " + newPlayer1.myBoard.shipList(indexShip).name)
                    }

                    if (newPlayer1.stillInGame) {
                        val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer)
                        mainLoopAIVSAI(newGameState)
                    } else {
                        val winner = gameState.player.copy(score = gameState.player.score+1 )
                        val newGS = gameState.copy(player = winner)
                        showQuestion(newGS.player.name + " Win")
                        showQuestion(newGS.player.name+" ; "+ newGS.player.score +" ; "+newGS.opponent.name+" ; "+ newGS.opponent.score)
                        return newGS
                    }
                } else {
                    showQuestion("Miss")
                    val newPlayer = gameState.player.makeAShoot(shootPosition)
                    val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)
                    if (gameState.opponent.stillInGame) {
                        showQuestion("My shoots")

                        val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer)
                        mainLoopAIVSAI(newGameState)
                    } else {
                        val winner = gameState.player.copy(score = gameState.player.score+1 )
                        val newGS = gameState.copy(player = winner)
                        showQuestion(newGS.player.name + " Win")
                        showQuestion(newGS.player.name+" ; "+ newGS.player.score +" ; "+newGS.opponent.name+" ; "+ newGS.opponent.score)
                        return newGS
                    }
                }
            }else{
                val winner = gameState.player.copy(score = gameState.player.score + 1)
                val newGS = gameState.copy(player = winner)
                showQuestion(newGS.player.name + " Win")
                showQuestion(newGS.player.name + " ; " + newGS.player.score + " ; " + newGS.opponent.name + " ; " + newGS.opponent.score)
                return newGS
            }

    }

    /**
      *
      */
    def selectMode(): Unit = {
        show()

        val mode = getUserIntInput()
        if (mode.nonEmpty) {
            mode.get match {
                case 1 => {
                    showQuestion("Enter first player name")
                    val name = getUserStringInput()
                    val newPlayer1 = createFleet(name, 1, true)

                    showQuestion("Enter second player name")
                    val name2 = getUserStringInput()
                    val newPlayer2 = createFleet(name2, 1, true)

                    val s = GameState(newPlayer1, newPlayer2, newPlayer1, mode.get,randomX, randomY, randomDir)
                    mainLoop(s)
                }

                case 2 => {
                    showQuestion("Enter first player name")
                    val name = getUserStringInput()
                    val newPlayer1 = createFleet(name, 5, true)

                    showQuestion("Creation of the AI-easy")
                    val newPlayer2 = createFleet("AI-easy", 5, false)

                    val s = GameState(newPlayer1, newPlayer2, newPlayer1, mode.get,randomX, randomY, randomDir)
                    mainLoop(s)
                }

                case 3 => {
                    showQuestion("Enter Human player name")
                    val name = getUserStringInput()
                    val newPlayer1 = createFleet(name, 5, true)

                    showQuestion("Creation of the AI-medium")
                    val newPlayer2 = createFleet("AI-medium", 5, false)

                    val s = GameState(newPlayer1, newPlayer2, newPlayer1, mode.get,randomX, randomY, randomDir)
                    mainLoop(s)
                }

                case 4 => {
                  showQuestion("Enter Human player name")
                  val name = getUserStringInput()
                  val newPlayer1 = createFleet(name, 5, true)

                  showQuestion("Creation of the AI-medium")
                  val newPlayer2 = createFleet("AI-hard", 5, false)

                  val s = GameState(newPlayer1, newPlayer2, newPlayer1, mode.get,randomX, randomY, randomDir)
                  mainLoop(s)
                }


                case 5 => {
                    showQuestion("Creation of proof file")
                    changeIAS()
                }
                case _ => {
                  showQuestion("Please pick a number for the  between 1 and 8")
                  selectMode()
                }
                /*case 6 => {
                  showQuestion("Creation of the AI-easy")
                  val newPlayer1 = createFleet("AI-easy", 5, false)

                  showQuestion("Creation of the AI-medium")
                  val newPlayer2 = createFleet("AI-medium", 5, false)

                  val s = GameState(newPlayer1, newPlayer2, newPlayer1, 5,randomX, randomY, randomDir)
                  val gS = mainLoopAIVSAI(s)
                  choice(gS)

                }
                case 7 => {
                  showQuestion("Creation of the AI-medium")
                  val newPlayer1 = createFleet("AI-medium", 5, false)

                  showQuestion("Creation of the AI-hard")
                  val newPlayer2 = createFleet("AI-hard", 5, false)

                  val s = GameState(newPlayer1, newPlayer2, newPlayer1, 5,randomX, randomY, randomDir)
                  val gS = mainLoopAIVSAI(s)
                  choice(gS)
                }
                case 8 => {
                  showQuestion("Creation of the AI-easy")
                  val newPlayer1 = createFleet("AI-easy", 5, false)

                  showQuestion("Creation of the AI-hard")
                  val newPlayer2 = createFleet("AI-hard", 5, false)

                  val s = GameState(newPlayer1, newPlayer2, newPlayer1, 5,randomX, randomY, randomDir)
                  val gS = mainLoopAIVSAI(s)
                  choice(gS)
                }*/
            }
        } else {
            showQuestion("Please pick a number for the  between 1 and 8")
            selectMode()
        }
        // handle the result
    }

    def changeIAS() : Unit = {
      val s = GameState(null, null, null, 5, randomDir, randomX, randomY)
      @tailrec
      def changeIASRec(gSRec : GameState, centaine : Int,myResult: List[Array[String]]): Unit = {
        if(centaine == 4){
          choice(gSRec)
          println()
        }else if(centaine == 1){
          showQuestion("Creation of the AI-easy")
          val newPlayer1 = createFleet("AI-easy", 5, false)

          showQuestion("Creation of the AI-medium")
          val newPlayer2 = createFleet("AI-medium", 5, false)

          val s = GameState(newPlayer1, newPlayer2, newPlayer1, 5,randomX, randomY, randomDir)
          val gS = NinetyNinetyGames(s,centaine,myResult)
          changeIASRec(gS._1,centaine+1,gS._2)

        }else if(centaine == 2) {
          showQuestion("Creation of the AI-medium")
          val newPlayer1 = createFleet("AI-medium", 5, false)

          showQuestion("Creation of the AI-hard")
          val newPlayer2 = createFleet("AI-hard", 5, false)

          val s = GameState(newPlayer1, newPlayer2, newPlayer1, 5,randomX, randomY, randomDir)
          val gS = NinetyNinetyGames(s,centaine,myResult)
          changeIASRec(gS._1,centaine+1,gS._2)

        }else if(centaine==3){
          showQuestion("Creation of the AI-easy")
          val newPlayer1 = createFleet("AI-easy", 5, false)

          showQuestion("Creation of the AI-hard")
          val newPlayer2 = createFleet("AI-hard", 5, false)

          val s = GameState(newPlayer1, newPlayer2, newPlayer1, 5,randomX, randomY, randomDir)
          val gS = NinetyNinetyGames(s,centaine,myResult)
          changeIASRec(gS._1,centaine+1,gS._2)

        }
      }

      changeIASRec(s,1,List())
    }

    def NinetyNinetyGames(gameState: GameState,centaine: Int,myResult : List[Array[String]]): (GameState,List[Array[String]]) = {
        @tailrec
        def NinetyNinetyGamesRec(gameStateRec: GameState,nGame: Int): (GameState,List[Array[String]]) ={
            if(nGame >= 100){
                val myResults =  myResult :+ Array(gameStateRec.player.name,gameStateRec.player.score.toString,gameStateRec.opponent.name,gameStateRec.opponent.score.toString)
                makeCSV(myResults)
              return (gameStateRec,myResults)
            }else{
                val newPlayer = createFleet(gameStateRec.player.name,5,gameStateRec.player.isHuman)
                val newOpponent = createFleet(gameStateRec.opponent.name,5,gameStateRec.opponent.isHuman)
                val newPlayerWithScore = newPlayer.copy(score = gameStateRec.player.score)
                val newOpponentWithScore = newOpponent.copy(score = gameStateRec.opponent.score)
                if(gameStateRec.beginner.name == newPlayer.name){
                    val newGameState = gameStateRec.copy(player = newOpponentWithScore,opponent = newPlayerWithScore,beginner = newOpponentWithScore)
                    val thisGameResult = mainLoopAIVSAI(newGameState)
                    NinetyNinetyGamesRec(thisGameResult,nGame+1)

                }else{
                    val newGameState = gameStateRec.copy(player = newPlayerWithScore,opponent = newOpponentWithScore,beginner = newPlayerWithScore)
                    val thisGameResult = mainLoopAIVSAI(newGameState)
                    NinetyNinetyGamesRec(thisGameResult,nGame+1)
                }
            }
        }

      NinetyNinetyGamesRec(gameState,0)
    }
}