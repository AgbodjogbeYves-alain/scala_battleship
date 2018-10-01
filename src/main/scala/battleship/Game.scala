package battleship

import scala.annotation.tailrec
import scala.util.Random
import scala.io.StdIn.readLine


case class GameState(player: Player, opponent: Player, beginner: Player)

object Battleship extends App {
    //val numbers = Array.ofDim[Int](10, 10)
    val shipColors = Array(Console.RED,Console.BLUE,Console.WHITE,Console.YELLOW,Console.GREEN)
    val boatSize = Array(2,3,3,4,5)
    val boatName = Array("Destroyer","Submarine","Cruiser","Battleship","Carrier")
    val r = Random

    selectMode()

    @tailrec
    def mainLoop(gameState: GameState) {
        showQuestion("Turn of player "+ gameState.player.name)

        //Imprimer mes 2 grilles
        showQuestion("Enter x position to shoot")
        val shootx = getUserIntInput().get

        showQuestion("Enter y position to shoot")
        val shooty = getUserIntInput().get

        val shootPosition = Position(shootx,shooty,false)

        if(shootPosition.isInGrid && gameState.player.stillInGame){
            val indexShip = gameState.opponent.myBoard.isItTouched(shootPosition).get
            if(indexShip != -1){
                showQuestion("Ship touched in ("+shootPosition.axisX+","+shootPosition.axisY+")")
                val newShootPosition = shootPosition.copy(isTouched = true)
                val newPlayer = gameState.player.makeAShoot(newShootPosition)
                val newPlayer1 = gameState.opponent.receiveAShoot(newShootPosition)


                if(newPlayer1.myBoard.shipList(indexShip).isSunk){
                    showQuestion("You sunk my "+ newPlayer1.myBoard.shipList(indexShip).name )
                }

                if(gameState.opponent.stillInGame){
                    val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer) 
                    mainLoop(newGameState)
                }
            }else{
                showQuestion("Miss")
                val newPlayer = gameState.player.makeAShoot(shootPosition)
                val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)
                if(gameState.opponent.stillInGame){
                    val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer) 
                    mainLoop(newGameState)
                }else{
                    showQuestion(gameState.player.name + " Win")
                }
            }

            
            
        }
            //Appeler la fonction du player qui est handleTir
            //Appeler la fonction du joueur courant qui tir sur l'opponent
            //Recup jthe new player and launch a new game with the inversed
        else if(shootx.equals(None) || shooty.equals(None)){
            showQuestion("Veuillez entrer des entiers pour les coordonnées de tir")
            mainLoop(gameState)
        }
        else if(!shootPosition.isInGrid){
            showQuestion("Veuillez entrer des coordonnées comprises entre 0 et 10")
            mainLoop(gameState)
        }else if(!gameState.player.stillInGame){
            showQuestion(gameState.opponent.name + " Win")
        }
        // handle the result
        /*userInput match {
            case "H" | "T" => {
                val coinTossResult = tossCoin(random)
                val newNumFlips = gameState.numFlips + 1
                if (userInput == coinTossResult) {
                    val newNumCorrect = gameState.numCorrect + 1
                    val newGameState = gameState.copy(numFlips = newNumFlips, numCorrect = newNumCorrect)
                    printGameState(printableFlipResult(coinTossResult), newGameState)
                    mainLoop(newGameState, random)
                } else {
                    val newGameState = gameState.copy(numFlips = newNumFlips)
                    printGameState(printableFlipResult(coinTossResult), newGameState)
                    mainLoop(newGameState, random)
                }
            }
            case _   => {
                printGameOver()
                printGameState(gameState)
                // return out of the recursion here
            }
        }*/
    }

    def selectMode() : Unit = {
        showMode()
        
        val mode = getUserIntInput()
         // handle the result
        mode.get match {
            case 1 => {                
                showQuestion("Enter first player name")
                val name = getUserStringInput
                val newPlayer1 = createFleet(name,5)

                showQuestion("Enter second player name")
                val name2 = getUserStringInput
                val newPlayer2 = createFleet(name2,5)
                val s = GameState(newPlayer1, newPlayer2, newPlayer2)
                mainLoop(s)
            }
        }



    }

    /*def enterPosition() : Option[Player] = {

    }*/

    def showMode() : Unit = {
        println("Enter number for the mode")
        println("1. Player VS Player")
        println("2. Player VS AI-easy")
        println("3. Player VS AI-medium")
        println("5. Player VS AI-hard")
        println("6. AI VS AI")
    }

    def showQuestion(message: String) : Unit = {
        println(message)
    }

    def isNumeric(input: String): Boolean = input.forall(_.isDigit)

    def getUserIntInput(): Option[Int] = {
        val value = readLine
        if(isNumeric(value)){
            Some(value.toInt)
        }else
            None
    }

    def getUserStringInput(): String = readLine.trim.toUpperCase

    def createFleet(name:String, nbShips: Int): Player = {
        //Si nbBateau == 0 return the new player with his list of ships update
        //Sinon recuperer le nom du bateau par rapport a un tableau avec les noms des bateaux, appeler le enterPosition qui
        //tourne tant que la position n'est pas valide et ensuite rappelle create fleet
        val BoardP = BBoard(List(),List(),List())
        val player = Player(name,BoardP)

        @tailrec
        def createFleetRec(playerRec:Player,nbShip: Int): Player = {
            if(nbShip==0){
                return playerRec
            }else{
                val newPlayer = enterPosition(player,boatSize(nbShip),boatName(nbShip))
                if(newPlayer==None){
                    showQuestion("The position that you enter is out of the grid or is already used by another Ship.")
                    createFleetRec(player,nbShip)
                }else{
                    showQuestion("Your "+boatName(nbShip)+" has been created")
                    createFleetRec(newPlayer.get,nbShip-1)

                }
            }
        }

        createFleetRec(player,nbShips)

    }

    def enterPosition(player: Player, size: Int, boatName: String ): Option[Player] = {
            showQuestion("Enter first position x of your "+boatName+"(size"+size+") ")
            val axisX = getUserIntInput()
            
            showQuestion("Enter first position y of your "+boatName+"(size"+size+")")
            val axisY = getUserIntInput()

            if(axisX.equals(None)||axisY.equals(None)){
                showQuestion("Please enter numeric value for positions")
                enterPosition(player,size,boatName)
            }
            showQuestion("Enter direction, H for horizontal and V for vertical "+boatName+"(size"+size+") ")
            val directionCarrier = getUserStringInput()

            val positionCarrier = Position(axisX.get,axisY.get,false)
            
            val redefinePlayer = player.createShip(boatName,positionCarrier,directionCarrier,size)

            return redefinePlayer
    }
}