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
case class GameState(player: Player, opponent: Player, beginner: Player, mode: Int)

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
    def mainLoop(gameState: GameState, randomX: Random, randomY: Random, randomDir: Random) {

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
                mainLoop(gameState, randomX, randomY, randomDir)
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
                            mainLoop(newGameState, randomX, randomY, randomDir)
                        } else {
                            showQuestion(gameState.player.name + " Win")
                            println()
                            choice(gameState)
                        }
                    } else {
                        showQuestion("Miss")
                        val newPlayer = gameState.player.makeAShoot(shootPosition)
                        val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)
                        if (gameState.opponent.stillInGame) {
                            val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer)
                            mainLoop(newGameState, randomX, randomY, randomDir)
                        } else {
                            showQuestion(gameState.player.name + " Win")
                        }
                    }


                }
                else if (!shootPosition.isInGrid) {
                    showQuestion("Please enter coordinate between 0 and 9")
                    mainLoop(gameState, randomX, randomY, randomDir)
                } else if (!gameState.opponent.stillInGame) {
                    showQuestion(gameState.opponent.name + " Win")
                }
            }
            //If the player is an AI
        } else {
            showQuestion("Ship AI grid")
            affichage(gameState.player)

            showQuestion("My shoots grid")
            showMyShoots(gameState.player)


            val shootPosition = gameState.player.getShootFromPlayer(randomX, randomY).get //Get the shoot from the AI
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
                        mainLoop(newGameState, randomX, randomY, randomDir)
                    } else {
                        showQuestion(gameState.player.name + " Win")
                        println()
                        choice(gameState)
                    }
                } else {
                    showQuestion("Miss")
                    val newPlayer = gameState.player.makeAShoot(shootPosition)
                    val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)
                    if (gameState.opponent.stillInGame) {
                        showQuestion("My shoots")

                        val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer)
                        mainLoop(newGameState, randomX, randomY, randomDir)
                    } else {
                        showQuestion(gameState.player.name + " Win")
                    }
                }
            }
            else if (!shootPosition.isInGrid) {
                mainLoop(gameState, randomX, randomY, randomDir)
            } else if (!gameState.opponent.stillInGame) {
                showQuestion(gameState.opponent.name + " Win")
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
    def mainLoopAIVSAI(gameState: GameState, randomX: Random, randomY: Random, randomDir: Random) {
            showQuestion("Ship AI grid")
            affichage(gameState.player)

            showQuestion("My shoots grid")
            showMyShoots(gameState.player)


            val shootPosition = gameState.player.getShootFromPlayer(randomX, randomY).get //Get the shoot from the AI
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
                        mainLoopAIVSAI(newGameState, randomX, randomY, randomDir)
                    } else {
                        showQuestion(gameState.player.name + " Win")
                        println()
                        choice(gameState)
                    }
                } else {
                    showQuestion("Miss")
                    val newPlayer = gameState.player.makeAShoot(shootPosition)
                    val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)
                    if (gameState.opponent.stillInGame) {
                        showQuestion("My shoots")

                        val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer)
                        mainLoopAIVSAI(newGameState, randomX, randomY, randomDir)
                    } else {
                        showQuestion(gameState.player.name + " Win")
                    }
                }
            }else if (!gameState.opponent.stillInGame) {
                showQuestion(gameState.opponent.name + " Win")
            }

    }

    /**
      *
      */
    def selectMode(): Unit = {
        show()

        val mode = getUserIntInput()
        if (!mode.isEmpty || mode.get > 6 || mode.get < 1) {
            mode.get match {
                case 1 => {
                    showQuestion("Enter first player name")
                    val name = getUserStringInput
                    val newPlayer1 = createFleet(name, 1, true)

                    showQuestion("Enter second player name")
                    val name2 = getUserStringInput
                    val newPlayer2 = createFleet(name2, 1, true)
                    val s = GameState(newPlayer1, newPlayer2, newPlayer1, mode.get)
                    mainLoop(s, randomX, randomY, randomDir)
                }

                case 2 => {
                    showQuestion("Enter first player name")
                    val name = getUserStringInput
                    val newPlayer1 = createFleet(name, 5, true)

                    showQuestion("Creation of the AI-easy")
                    val newPlayer2 = createFleet("AI-easy", 5, false)
                    val s = GameState(newPlayer1, newPlayer2, newPlayer1, mode.get)
                    mainLoop(s, randomX, randomY, randomDir)
                }

                case 3 => {
                    showQuestion("Enter Human player name")
                    val name = getUserStringInput
                    val newPlayer1 = createFleet(name, 5, true)

                    showQuestion("Creation of the AI-medium")
                    val newPlayer2 = createFleet("AI-medium", 5, false)
                    val s = GameState(newPlayer1, newPlayer2, newPlayer1, mode.get)
                    mainLoop(s, randomX, randomY, randomDir)
                }

                case 4 => {
                    showQuestion("TODO Human vs hard")
                    /*val name = getUserStringInput
                    val newPlayer1 = createFleet(name, 5, true)

                    showQuestion("Creation of the AI-medium")
                    val newPlayer2 = createFleet("AI-medium", 5, false)
                    val s = GameState(newPlayer1, newPlayer2, newPlayer1, mode.get)
                    mainLoop(s, randomX, randomY, randomDir)*/
                }

                case 5 => {
                    showQuestion("Creation of the AI-easy")
                    val newPlayer1 = createFleet("AI-easy", 5, false)

                    showQuestion("Creation of the AI-medium")
                    val newPlayer2 = createFleet("AI-medium", 5, false)
                    val s = GameState(newPlayer1, newPlayer2, newPlayer1, mode.get)
                    mainLoop(s, randomX, randomY, randomDir)
                }

                case 6 => {
                    showQuestion("Creation of the AI-medium")
                    val newPlayer1 = createFleet("AI-medium", 5, false)

                    showQuestion("Creation of the AI-hard")
                    val newPlayer2 = createFleet("AI-hard", 5, false)
                    val s = GameState(newPlayer1, newPlayer2, newPlayer1, mode.get)
                    mainLoop(s, randomX, randomY, randomDir)
                }
            }
        } else {
            showQuestion("Please pick a number for the  between 1 and 7")
            selectMode()
        }
        // handle the result
    }
}