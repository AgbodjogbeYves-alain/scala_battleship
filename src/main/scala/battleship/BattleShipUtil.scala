package battleship

import battleship.Battleship._

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Try

object BattleShipUtil {
  /**
    *
    */
  def show() : Unit = {
    println("Enter number for the ")
    println("1. Player VS Player")
    println("2. Player VS AI-easy")
    println("3. Player VS AI-medium")
    println("4. Player VS AI-hard")
    println("5. AI-easy VS AI-medium")
    println("6. AI-medium VS AI-hard")
    println("7. AI-hard VS AI-easy")

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
  def createFleet(name:String, nbShips: Int,playerType: Boolean): Player = {
    //Si nbBateau == 0 return the new player with his list of ships update
    //Sinon recuperer le nom du bateau par rapport a un tableau avec les noms des bateaux, appeler le enterPosition qui
    //tourne tant que la position n'est pas valide et ensuite rappelle create fleet
    val BoardP = BBoard(List(),List(),List())
    val player = Player(name,BoardP,playerType)
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
    if(player.isHuman){
      showQuestion("Enter first position x of your "+boatName+"(size "+size+") ")
      val axisX = getUserIntInput()

      showQuestion("Enter first position y of your "+boatName+"(size "+size+")")
      val axisY = getUserIntInput()

      if(!axisX.isEmpty && !axisY.isEmpty){
        showQuestion("Enter direction, H for horizontal and V for vertical "+boatName+" (size "+size+") ")
        val directionCarrier = getUserStringInput()

        val positionCarrier = Position(axisX.get,axisY.get,false)

        if(positionCarrier.isInGrid){
          val redefinePlayer = player.createShip(boatName,positionCarrier,directionCarrier,size)
          return redefinePlayer
        }else{
          showQuestion("Please enter a number for position x and y between 0 and 9")
          enterPosition(player,size,boatName)
        }
      }else{
        showQuestion("Please enter a number for position x and y between 0 and 9")
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
          if(pos!="N/A" && pos!="-1" && pos!="MS"){
            print(Console.BLUE_B+Console.BLACK+" "+pos+Console.BLACK+" "+Console.BLUE_B+Console.BLACK_B+"  ")
          }else if(pos=="N/A"){
            print(pos + "  ")
          }else if(pos=="-1"){
            print(Console.RED_B+" "+pos+Console.RED_B+Console.BLACK_B+" ")
          }else if(pos=="MS"){
            print(Console.MAGENTA_B+Console.BLACK+" "+pos+Console.BLACK+" "+Console.MAGENTA_B+Console.BLACK_B+" ")
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
      val newPlayer = createFleet(gameState.player.name,5,gameState.player.isHuman)
      val newOpponent = createFleet(gameState.opponent.name,5,gameState.opponent.isHuman)
      if(gameState.beginner.name == newPlayer.name){
        val newGameState = gameState.copy(player = newOpponent,opponent = newPlayer,beginner = newOpponent)
        mainLoop(newGameState,randomX, randomY,randomDir)
      }else{
        val newGameState = gameState.copy(player = newPlayer,opponent = newOpponent,beginner = newPlayer)
        mainLoop(newGameState,randomX, randomY,randomDir)

      }
    }else if(choice == "M"){
      selectMode()
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
            print(Console.MAGENTA_B+" "+"MO"+" "+Console.MAGENTA_B+Console.BLACK_B+"  ")
          }
        })
        println()
        showMyShootRec(numLine+1)
      }
    }

    showMyShootRec(0)
  }
  //Get the shoot from the AI

}
