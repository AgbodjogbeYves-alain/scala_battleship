package battleship

import battleship.Battleship._

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Try
import java.io.{BufferedWriter, FileWriter}
import scala.collection.JavaConversions._
import au.com.bytecode.opencsv.CSVWriter

object BattleShipUtil {
  /**
    * Show the modes
    */
  def show() : Unit = {
    println("Enter number for the ")
    println("1. Player VS Player")
    println("2. Player VS AI-easy")
    println("3. Player VS AI-medium")
    println("4. Player VS AI-hard")
    println("5. Proof file")
    /*println("6. AI-easy VS AI-medium")//Medium vs easy
    println("7. AI-medium VS AI-hard")//Medium vs easy
    println("8. AI-hard VS AI-easy")//Medium vs easy*/



  }

  /**
    * Method to display a message to the user
    * @param message : String : Message to display
    */
  def showQuestion(message: String) : Unit = {
    println()
    println(message)
    println()
  }

  /**
    * Method to get a int from the user
    * @return : Option[Int] : Some(Int) if the user enter a number else None
    */
  def getUserIntInput() : Option[Int] = {
    val input = readLine
    if(Try(input.toDouble).isSuccess)
      return Some(input.toInt)
    else
      return None
  }

  /**
    * Method to get user string input
    * @return : String
    */
  def getUserStringInput(): String = readLine.trim.toUpperCase


  /**
    * Method to create a player fleet
    * @param name : Player Name
    * @param nbShips : Ship number
    * @return : Player : A new player with his fleet
    */
  def createFleet(name:String, nbShips: Int,playerType: Boolean): Player = {
    //Si nbBateau == 0 return the new player with his list of ships update
    //Sinon recuperer le nom du bateau par rapport a un tableau avec les noms des bateaux, appeler le enterPosition qui
    //tourne tant que la position n'est pas valide et ensuite rappelle create fleet
    val BoardP = BBoard(List(),List(),List())
    val player = Player(name,BoardP,playerType)
    showQuestion("Hi "+name+". Please enter the following informations")

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
    * Method to ask the player for the first up or left position of a ship and the direction
    * @param player : Player : The concerned player
    * @param size : Int : Ship size
    * @param boatName : String  :Ship name
    * @return : Option[Player] : A new player with a new Ship
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

        if(positionCarrier.isInGrid && (directionCarrier=="V" || directionCarrier == "H")){
          val redefinePlayer = player.createShip(boatName,positionCarrier,directionCarrier,size)
          return redefinePlayer
        }else{
          showQuestion("Please enter a number for position x and y between 0 and 9; And for the direction H for horizontally or V for vertically")
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
    * Method to  display player ship grid
    * @param player : Player : The player for who we want to display the ship grid
    */
  def affichage(player: Player){
    val grid = player.createMyGridForShow()

    println("    0    1    2    3    4    5    6    7    8    9")
    println("----------------------------------------------------------")

    /**
      * Tail rec method which iterate on each line of the player grid
      * @param numLine : Int : Number of line
      */
    @tailrec
    def affichageRec(numLine: Int) : Unit = {
      if(numLine == player.createMyGridForShow.size){
        return
      }else{
        val currentLigne = grid(numLine)
        print(numLine+ "  ")
        currentLigne.foreach(pos => {
          if(pos!="N/A" && pos!="-1" && pos!="MIS"){
            print(Console.BLUE_B+Console.BLACK+" "+pos+Console.BLACK+" "+Console.BLUE_B+Console.BLACK_B+"  ")
          }else if(pos=="N/A"){
            print(pos + "  ")
          }else if(pos=="-1"){
            print(Console.RED_B+" "+pos+Console.RED_B+Console.BLACK_B+"  ")
          }else if(pos=="MIS"){
            print(Console.MAGENTA_B+pos+Console.BLACK_B+"  ")
          }
        })
        println()
        affichageRec(numLine+1)
      }
    }

    affichageRec(0)
  }

  /**
    * Method to Seleect what to do at the end of a game
    * @param gameState : GameState : The previous gameState
    */
  def choice(gameState : GameState) : Unit = {
    showQuestion("What want you do ? R for restart this mode, M for play another mode , Everything else for exit")
    val choice = getUserStringInput()
    if(choice=="R"){
      val newPlayer = createFleet(gameState.player.name,5,gameState.player.isHuman)
      val newOpponent = createFleet(gameState.opponent.name,5,gameState.opponent.isHuman)
      val newPlayerWithScore = newPlayer.copy(score = gameState.player.score)
      val newOpponentWithScore = newOpponent.copy(score = gameState.opponent.score)

      if(gameState.beginner.name == newPlayer.name){
        val newGameState = gameState.copy(player = newOpponentWithScore,opponent = newPlayerWithScore,beginner = newOpponentWithScore)
        if(newGameState.player.isHuman || newGameState.opponent.isHuman){
          mainLoop(newGameState)
        }else{
          changeIAS()
        }
      }else{
        val newGameState = gameState.copy(player = newPlayerWithScore,opponent = newOpponentWithScore,beginner = newPlayerWithScore)
        if(newGameState.player.isHuman || newGameState.opponent.isHuman){
          mainLoop(newGameState)
        }else{
          changeIAS()
        }

      }
    }else if(choice == "M"){
      selectMode()
    }else{
      showQuestion("Thanks for this game")
    }

  }

  /**
    * Method to display player shoots
    * @param player : Player : The concerned player
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
            print(Console.RED_B+" "+pos+Console.RED_B+Console.BLACK_B+"  ")
          }else if(pos=="N/A"){
            print(pos + "  ")
          }else{
            print(Console.MAGENTA_B+"MIS"+Console.BLACK_B+"  ")
          }
        })
        println()
        showMyShootRec(numLine+1)
      }
    }

    showMyShootRec(0)
  }
  //Get the shoot from the AI

  /**
    * Method to create my csv proof file
    * @param myScores : List[Array[String\]\] : List with all the array of the game results
    */
  def makeCSV(myScores : List[Array[String]]): Unit = {
    val outputFile = new BufferedWriter(new FileWriter("./ai_proof.csv")) //replace the path with the desired path and filename with the desired filename
    val csvWriter = new CSVWriter(outputFile)
    csvWriter.writeAll(myScores)
    csvWriter.close()
  }
}
