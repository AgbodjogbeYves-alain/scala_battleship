package battleship

import scala.annotation.tailrec
import scala.util.Random
import scala.io.StdIn.readLine
import scala.util.Try
import Console.{RED_B,YELLOW_B,BLUE_B,WHITE_B,GREEN_B,CYAN_B}

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

    val boatSize = Array(2,3,3,4,5)
    val boatName = Array("Destroyer","Submarine","Cruiser","Battleship","Carrier")
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
    def mainLoop(gameState: GameState,randomX: Random, randomY: Random,randomDir: Random ) {

        showQuestion("Turn of player "+ gameState.player.name)

        //If the player is not an AI
        if(gameState.player.name.take(2) != "AI"){
            showQuestion(gameState.player.name + "ships grid")
            affichage(gameState.player)

            showQuestion(gameState.player.name + "shoots grid")
            showMyShoots(gameState.player)

            showQuestion("Enter x position to shoot")
            val shootx = getUserIntInput()
            
            showQuestion("Enter y position to shoot")
            val shooty = getUserIntInput()

            if(shootx.isEmpty || shooty.isEmpty){
                mainLoop(gameState,randomX,randomY,randomDir)
            }else{
                val shootPosition = Position(shootx.get,shooty.get,false)

                if(shootPosition.isInGrid && gameState.opponent.stillInGame){
                    val indexShip = gameState.opponent.myBoard.isItTouched(shootPosition).get
                    if(indexShip != -1){
                        showQuestion("Ship touched in ("+shootPosition.axisX+","+shootPosition.axisY+")")
                        val newShootPosition = shootPosition.copy(isTouched = true)
                        val newPlayer = gameState.player.makeAShoot(newShootPosition)
                        val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)

                        if(newPlayer1.myBoard.shipList(indexShip).isSunk){
                            showQuestion("You sunk "+ newPlayer1.name+" " + newPlayer1.myBoard.shipList(indexShip).name )
                        }

                        if(newPlayer1.stillInGame){
                            val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer) 
                            mainLoop(newGameState,randomX,randomY,randomDir)
                        }else{
                            showQuestion(gameState.player.name + " Win")
                            println()
                            choice(gameState)
                        }
                    }else{
                        showQuestion("Miss")
                        val newPlayer = gameState.player.makeAShoot(shootPosition)
                        val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)
                        if(gameState.opponent.stillInGame){
                            val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer)
                            mainLoop(newGameState,randomX,randomY,randomDir)
                        }else{
                            showQuestion(gameState.player.name + " Win")
                        }
                    }

                    
                    
                }
                else if(!shootPosition.isInGrid){
                    showQuestion("Please enter coordinate between 0 and 9")
                    mainLoop(gameState,randomX, randomY,randomDir)
                }else if(!gameState.opponent.stillInGame){
                    showQuestion(gameState.opponent.name + " Win")
                }
            }
        //If the player is an AI
        }else{
            showQuestion("Ship AI grid")
            affichage(gameState.player)

            showQuestion("My shoots grid")
            showMyShoots(gameState.player)

            val shootPosition = gameState.player.shootPosition(randomX,randomY).get//Get the shoot from the AI
            if(shootPosition.isInGrid && gameState.opponent.stillInGame){
                val indexShip = gameState.opponent.myBoard.isItTouched(shootPosition).get
                if(indexShip != -1){
                    showQuestion("Ship touched in ("+shootPosition.axisX+","+shootPosition.axisY+")")
                    val newShootPosition = shootPosition.copy(isTouched = true)
                    val newPlayer = gameState.player.makeAShoot(newShootPosition)
                    val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)

                    if(newPlayer1.myBoard.shipList(indexShip).isSunk){
                        showQuestion("You sunk "+ newPlayer1.name+" " + newPlayer1.myBoard.shipList(indexShip).name )
                    }

                    if(newPlayer1.stillInGame){
                        val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer) 
                        mainLoop(newGameState,randomX,randomY,randomDir)
                    }else{
                        showQuestion(gameState.player.name + " Win")
                        println()
                        choice(gameState)
                    }
                }else{
                    showQuestion("Miss")
                    val newPlayer = gameState.player.makeAShoot(shootPosition)
                    val newPlayer1 = gameState.opponent.receiveAShoot(shootPosition)
                    if(gameState.opponent.stillInGame){
                        showQuestion("My shoots")

                        val newGameState = gameState.copy(player = newPlayer1, opponent = newPlayer)
                        mainLoop(newGameState,randomX, randomY,randomDir)
                    }else{
                        showQuestion(gameState.player.name + " Win")
                    }
                } 
            }
            else if(!shootPosition.isInGrid){
                showQuestion("Veuillez entrer des coordonnées comprises entre 0 et 10")
                mainLoop(gameState,randomX, randomY,randomDir)
            }else if(!gameState.opponent.stillInGame){
                showQuestion(gameState.opponent.name + " Win")
            }
            
        }
        
    }

    /**
      *
      */
    def selectMode() : Unit = {
        show()
        
        val mode = getUserIntInput()
        if(!mode.isEmpty || mode.get>6 || mode.get<1){
            mode.get match {
                case 1 => {                
                    showQuestion("Enter first player name")
                    val name = getUserStringInput
                    val newPlayer1 = createFleet(name,1)

                    showQuestion("Enter second player name")
                    val name2 = getUserStringInput
                    val newPlayer2 = createFleet(name2,1)
                    val s = GameState(newPlayer1, newPlayer2, newPlayer1,mode.get)
                    mainLoop(s,randomX, randomY,randomDir)
                }

                case 2 => {
                    showQuestion("Enter first player name")
                    val name = getUserStringInput
                    val newPlayer1 = createFleet(name,1)

                    showQuestion("Creation of the AI-easy")
                    val newPlayer2 = createFleet("AI-easy",5)
                    val s = GameState(newPlayer1, newPlayer2, newPlayer1,mode.get)
                    mainLoop(s,randomX, randomY,randomDir)
                }
            }
        }else{
            showQuestion("Please pick a number for the  between 1 to 6")
            ()
        }
         // handle the result
        



    }

    /*def enterPosition() : Option[Player] = {

    }*/

    /**
      *
      */
    def show() : Unit = {
        println("Enter number for the ")
        println("1. Player VS Player")
        println("2. Player VS AI-easy")
        println("3. Player VS AI-medium")
        println("5. Player VS AI-hard")
        println("6. AI-easy VS AI-medium")
        println("6. AI-medium VS AI-hard")
        println("6. AI-hard VS AI-easy")

    }

    /**
      *
      * @param message
      */
    def showQuestion(message: String) : Unit = {
        println()
        println(message)
        println()
    }

//    def isNumeric(input: String): Boolean = input.forall(_.isDigit)

    /**
      *
      * @return
      */
    def getUserIntInput() : Option[Int] = {
        val input = readLine
        if(Try(input.toDouble).isSuccess)
            return Some(input.toInt)
        else    
            return None    
    }

    /**
      *
      * @return
      */
    def getUserStringInput(): String = readLine.trim.toUpperCase


    /**
      *
      * @param name
      * @param nbShips
      * @return
      */
    def createFleet(name:String, nbShips: Int): Player = {
        //Si nbBateau == 0 return the new player with his list of ships update
        //Sinon recuperer le nom du bateau par rapport a un tableau avec les noms des bateaux, appeler le enterPosition qui
        //tourne tant que la position n'est pas valide et ensuite rappelle create fleet
        val BoardP = BBoard(List(),List(),List())
        val player = Player(name,BoardP)
        showQuestion("Hi "+name+". Please enter the following informations")

        /**
          *
          * @param playerRec
          * @param nbShipRec
          * @return
          */
        @tailrec
        def createFleetRec(playerRec:Player,nbShipRec: Int): Player = {
            if(nbShipRec==0){
                return playerRec
            }else{
                val newPlayer = enterPosition(playerRec,boatSize(nbShipRec-1),boatName(nbShipRec-1))
                if(newPlayer.isEmpty){
                    showQuestion("The positions for your ship is out of the grid or is already used by another Ship.")
                    createFleetRec(playerRec,nbShipRec)
                }else{
                    showQuestion("Your "+boatName(nbShipRec-1)+" has been created")
                    createFleetRec(newPlayer.get,nbShipRec-1)

                }
            }
        }

        createFleetRec(player,nbShips)

    }

    /**
      *
      * @param player
      * @param size
      * @param boatName
      * @return
      */
    def enterPosition(player: Player, size: Int, boatName: String ): Option[Player] = {
        if(player.name.take(2)!="AI"){
            showQuestion("Enter first position x of your "+boatName+"(size "+size+") ")
            val axisX = getUserIntInput()
            
            showQuestion("Enter first position y of your "+boatName+"(size "+size+")")
            val axisY = getUserIntInput()

            if(!axisX.isEmpty && !axisY.isEmpty){
                showQuestion("Enter direction, H for horizontal and V for vertical "+boatName+" (size "+size+") ")
                val directionCarrier = getUserStringInput()

                val positionCarrier = Position(axisX.get,axisY.get,false)
            
                val redefinePlayer = player.createShip(boatName,positionCarrier,directionCarrier,size)
                return redefinePlayer
            }else{
                showQuestion("Please enter a number for position x and y")
                enterPosition(player,size,boatName)
            }
        }else{
            val position = player.generateRandomPosition(randomX,randomY)
            val direction = player.generateRandomDirection(randomDir)
            val redefinePlayer = player.createShip(boatName,position,direction,size)
            return redefinePlayer
        }
            
            
    }

    /**
      *
      * @param player
      */
    def affichage(player: Player){
        val grid = player.createMyGridForShow()

        println("    0    1    2    3    4    5    6    7    8    9")
        println("----------------------------------------------------------")

        /**
          *
          * @param numLine
          */
        @tailrec
        def affichageRec(numLine: Int) : Unit = {
            if(numLine == player.createMyGridForShow.size){
                return 
            }else{
                val currentLigne = grid(numLine)
                print(numLine+ "  ")
                currentLigne.foreach(pos => {
                    if(pos!="N/A" && pos!="-1" && pos!="MO"){
                        print(Console.BLUE_B+" "+pos+" "+Console.BLUE_B+Console.BLACK_B+"  ")
                    }else if(pos=="N/A"){
                        print(pos + "  ")
                    }else if(pos=="-1"){
                        print(Console.RED_B+" "+pos+" "+Console.RED_B+Console.BLACK_B+" ")
                    }else if(pos=="MO"){
                        print(Console.MAGENTA_B+" "+pos+" "+Console.MAGENTA_B+Console.BLACK_B+" ")
                    }
                })
                println()
                affichageRec(numLine+1)
            }
        }
        
        affichageRec(0)
    }

    /**
      *
      * @param gameState
      */
    def choice(gameState : GameState) : Unit = {
         showQuestion("What want you do ? R for restart this , M to  another , E for exit")
         val choice = getUserStringInput()
         if(choice=="R"){
            val newPlayer = createFleet(gameState.player.name,1)
            val newOpponent = createFleet(gameState.opponent.name,1)
            if(gameState.beginner.name == newPlayer.name){
                val newGameState = gameState.copy(player = newOpponent,opponent = newPlayer,beginner = newOpponent)
                mainLoop(newGameState,randomX, randomY,randomDir)
            }else{
                val newGameState = gameState.copy(player = newPlayer,opponent = newOpponent,beginner = newPlayer)
                mainLoop(newGameState,randomX, randomY,randomDir)

            }
         }else if(choice == "M"){
            println()
            ()
         }else if(choice == "E"){
            showQuestion("Thanks for this game")
         }
                         
    }

    /**
      *
      * @param player
      */
    def showMyShoots(player: Player) : Unit = {
        val grid = player.createMyShootGridForShow()

        println("    0    1    2    3    4    5    6    7    8    9")
        println("----------------------------------------------------------")

        /**
          *
          * @param numLine
          */
        @tailrec
        def showMyShootRec(numLine: Int) : Unit = {
            if(numLine == player.createMyShootGridForShow.size){
                return 
            }else{
                val currentLigne = grid(numLine)
                print(numLine+ "  ")
                currentLigne.foreach(pos => {
                    if(pos =="-1"){
                        print(Console.RED_B+pos+" "+Console.RED_B+Console.BLACK_B+"  ")
                    }else if(pos=="N/A"){
                        print(pos + "  ")
                    }else{
                        print(Console.BLUE_B+" "+" "+" "+Console.BLUE_B+Console.BLACK_B+"  ")
                    }
                })
                println()
                showMyShootRec(numLine+1)
            }
        }
    
        showMyShootRec(0)
    } 
}