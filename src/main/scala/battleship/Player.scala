package battleship
import scala.annotation.tailrec
import scala.util.Random


/**
  * Class Player. Represent the player in a party which can be human or IA
  * @param name : String : Player's Name
  * @param myBoard : BBoard : Board of the player
  * @param myType : Boolean : True if the player is human else false
  * @param score : Int : Score of the player
  */
case class Player(name: String,myBoard: BBoard,isHuman: Boolean,score: Int=0) {

  /**
    * Method to handle a shoot from an opponent
    * @param position : Position : Shoot position of the opponent
    * @return : Player : A new Player with the new Board
    */
  def receiveAShoot(position: Position): Player = {
    val player = copy(myBoard = myBoard.aNewOpponentShoot(position).get)
    return player
    //Handle the fact that the opponent shoot you. If the player touch me i return a new player with the position add to opponentsShoot
    //and position in the ship pass to touch else a none
  }

  /**
    * Method to add my new shoot to the opponent
    * @param position : Position : My shoot position
    * @return : Player : A new player with an update Bord myShoot List
    */
  def makeAShoot(position: Position): Player = {
    val player = copy(myBoard = myBoard.myNewShoot(position).get)
    return player
  }

  /**
    * Method to create a a new ship and add it to the Player Board
    * @param name : String : Ship name (Carrier,Destroyer, etc ....)
    * @param position : Position : The first left or up position of the ship
    * @param direction : String : Direction of the ship --> H for horizontally or V for Vertically
    * @param size : Int : Ship size
    * @return : Option[Player] : If the creation was ended correctly (if the position of the ships not already exist or not out of the grid)
    *         the method returns Some(Player) and the player is a new Player with the ship added else the Method returns None
    */
  def createShip(name: String, position: Position, direction: String, size: Int): Option[Player] = {

    val listPosition = List()

    if (myBoard.checkIfPositionAlreadyExist(position).get != -1 || !position.isInGrid) {
      return None
    } else {
      @tailrec
      def createShipRec(name: String, position: Position, listPosition: List[Position], direction: String, size: Int): Option[Player] = {
        if (size == 0) {//Accumulateur tailrec. Creation of position until the size is fill
          val newShip = Ship(name, listPosition.size, false, listPosition)
          val newBBoard = myBoard.addNewShip(newShip)
          val newPlayer = copy(myBoard = newBBoard)
          return Some(newPlayer)
        } else {
          if (direction == "V") {//Creation if the direction is V
            val newPosition = Position(position.axisX, position.axisY + size - 1, false)
            if (myBoard.checkIfPositionAlreadyExist(newPosition).get == -1 && newPosition.isInGrid) {
              val newListPosition = newPosition :: listPosition
              createShipRec(name, position, newListPosition, "V", size - 1)
            } else {
              return None
            }
          }

          else {//Creation if the direction is H
            val newPosition = Position(position.axisX + size - 1, position.axisY, false)
            if (myBoard.checkIfPositionAlreadyExist(newPosition).get == -1 && newPosition.isInGrid) {
              val newListPosition = newPosition :: listPosition
              createShipRec(name, position, newListPosition, "H", size - 1)
            } else {
              return None
            }

          }

        }
      }

      createShipRec(name, position, listPosition, direction, size)//Tail recursion while the size is not fill

    }


  }

  /**
    * Method to check if the player is sill in game --> if all is ship are not sunk
    * @return : Boolean : True if all ship are not sunk else false
    */
  def stillInGame(): Boolean = {
    return !myBoard.noShipLeft.get
  }

  /**
    * Method to create the grid which will be displayed in front
    * @return : List[List[String/]/] : Represent a grid format with the size of the boat has and the string are the size of the boats or (-1 or MIS for the opponent shoots)
    */
  def createMyGridForShow(): List[List[String]] = {
    val grid = List.fill(10)(List.fill(10)("N/A"))

    /**
      * Tail rec method to iterate on the ship list
      * @param grid : List[List[String/]/] : The grid with all position
      * @param shipList : List[Ship] :: THe player list of ships
      * @param remainShip : Int : Number of ship which are not placed already in the grid
      * @return : List[List[String/]/] : Returnt the grid with all the ships position and opponents shoot represent as strings
      */
    @tailrec
    def createMyGridForShowTailRec(gridRec: List[List[String]], shipList: List[Ship], remainShip: Int): List[List[String]] = {
      if (remainShip < 0) {
        return gridRec
      } else {
        val showedShipPositions = shipList(remainShip).positionList
        val newGrid = defineNewGrid(showedShipPositions, gridRec)
        val finalGrid = addOpponentsShootsGrid(myBoard.opponentShoots, newGrid)
        createMyGridForShowTailRec(finalGrid, myBoard.shipList, remainShip - 1)
      }
    }

    createMyGridForShowTailRec(grid, myBoard.shipList, myBoard.shipList.size - 1)

  }

  /**
    * Method to fill the grid sith positions of one ship
    * @param positionList : List[Position] : Ship position list
    * @param grid : List[List[String/]/] : The previous grid which need to be updated
    * @return : List[List[String/]/] : The new grid with all the position of one ship represented as strings
    */
  def defineNewGrid(positionList: List[Position], grid: List[List[String]]): List[List[String]] = {

    /**
      * Tail rec method which iterate on each position
      * @param gridRec : List[List[String/]/] : The grid with all positiions
      * @param remainsPosition : Int : Number of position which are not already placed in the grid
      * @return : List[List[String/]/] : The grid with all the position of a ship placed
      */
    @tailrec
    def defineNewGridRec(gridRec: List[List[String]], remainsPosition: Int): List[List[String]] = {
      if (remainsPosition < 0) {//If the remains position is under 0 the we finished to placed the position
        return gridRec
      } else {
        val position = positionList(remainsPosition)
        val axisYList = gridRec(position.axisY)
        if (position.isTouched) {
          val newAxisYList = axisYList.updated(position.axisX, "-1")//If the positon was touched by an opponent shoot we put -1
          val newGrid = gridRec.updated(position.axisY, newAxisYList)
          defineNewGridRec(newGrid, remainsPosition - 1)
        } else {
          val newAxisYList = axisYList.updated(position.axisX, "" + positionList.size)//We put size of the boat
          val newGrid = gridRec.updated(position.axisY, newAxisYList)
          defineNewGridRec(newGrid, remainsPosition - 1)
        }
      }
    }

    defineNewGridRec(grid, positionList.size - 1)//We call the tail rec function there
  }

  /**
    * Method to create the shoot grid of the player. the grid with the positions where he shoots
    * @return : List[List[String/]/] : Grid with string with -1 if the shoot touch the oppnent else 1
    */
  def createMyShootGridForShow(): List[List[String]] = {
    val grid = List.fill(10)(List.fill(10)("N/A"))

    @tailrec
    def createMyShootGridForShowRec(grid: List[List[String]], shootListSize: Int): List[List[String]] = {
      if (shootListSize < 0) {
        return grid
      } else {
        val position = myBoard.myShoots(shootListSize)
        val axisYList = grid(position.axisY)
        if (position.isTouched) {
          val newAxisYList = axisYList.updated(position.axisX, "-1")
          val newGrid = grid.updated(position.axisY, newAxisYList)
          createMyShootGridForShowRec(newGrid, shootListSize - 1)
        } else {
          val newAxisYList = axisYList.updated(position.axisX, "1")
          val newGrid = grid.updated(position.axisY, newAxisYList)
          createMyShootGridForShowRec(newGrid, shootListSize - 1)
        }
      }
    }

    createMyShootGridForShowRec(grid, myBoard.myShoots.size - 1)

  }

  /**
    * Method to add to my ship grid the opponents shoots
    * @param positionList : List[Position] : WHere my opponents shoot
    * @param grid : List[List[String/]/] : The grid with the ships
    * @return List[List[String/]/] : A new grid with the opponents shoots -1 if  the position is touched else MIS
    */
  def addOpponentsShootsGrid(positionList: List[Position], grid: List[List[String]]): List[List[String]] = {

    @tailrec
    def addOpponentsShootsGridRec(gridRec: List[List[String]], remainsPosition: Int): List[List[String]] = {
      if (remainsPosition < 0) {
        return gridRec
      } else {
        val position = positionList(remainsPosition)
        val axisYList = gridRec(position.axisY)
        if (!position.isTouched) {
          val newAxisYList = axisYList.updated(position.axisX, "MIS")
          val newGrid = gridRec.updated(position.axisY, newAxisYList)
          addOpponentsShootsGridRec(newGrid, remainsPosition - 1)
        } else {
          val newAxisYList = axisYList.updated(position.axisX, "-1")
          val newGrid = gridRec.updated(position.axisY, newAxisYList)
          addOpponentsShootsGridRec(gridRec, remainsPosition - 1)
        }
      }
    }

    addOpponentsShootsGridRec(grid, positionList.size - 1)
  }

  /**
    * Method to generate a Random position.Use for the creation of the AI fleet
    * @param randX : Random
    * @param randY : Random
    * @return : Position : First up or left position of a boat
    */
  def generateRandomPosition(randX: Random, randY: Random): Position = {
    val axisX = randX.nextInt(10)
    val axisY = randY.nextInt(10)
    val generatedPosition = Position(axisX, axisY, false)
    return generatedPosition

  }

  /**
    * Method to generate a V or H direction randomly
    * @param randDir : Random
    * @return : String : H if the random int mod 2 != 0 else V
    */
  def generateRandomDirection(randDir: Random): String = {
    val dir = randDir.nextInt
    if (dir % 2 == 0) {
      return "V"
    } else {
      return "H"
    }
  }

  /**
    * Shoot method for easy AI
    * @param randX : Random
    * @param randY : Random
    * @return : Position : Position to shoot
    */
  def shootPositionEasy(randX: Random, randY: Random): Position = {
    return Position(randX.nextInt(10), randY.nextInt(10), false)//It's just a random shoot
  }

  /**
    * Shoot method for the Medium AI
    * @param randX :Random
    * @param randY : Random
    * @return : Position : Shoot position
    */
  def shootPositionMedium(randX: Random, randY: Random): Position = {
    val missedShoot = myBoard.myShoots.filter(pos => pos.isTouched == false)
    val shootPosition = Position(randX.nextInt(10), randY.nextInt(10), false)//We just check if the position not already shoot
    if (missedShoot.forall(pos => !pos.equals(shootPosition))) {
      shootPosition
    } else {
      shootPositionMedium(randX, randY)
    }
  }


  /**
    * Shoot method for the hard AI
    * @param randX : Random
    * @param randY : Random
    * @return : Position : The shoot position
    */
  def shootPositionHard(randX: Random, randY: Random): Position = {
    val mySucceedShoots = myBoard.myShoots.filter(pos => pos.isTouched)
    if (myBoard.myShoots.nonEmpty && mySucceedShoots.nonEmpty) {//If AI succeed at least 1 shoot
      val previousTouchShoot = mySucceedShoots.head//We gett the previous touched shoot
        val newShoot = previousTouchShoot.copy(axisX = previousTouchShoot.axisX + 1,isTouched = false)//We create a new shoot just at the right of the previous
        if (newShoot.isInGrid && myBoard.myShoots.forall(pos => !pos.equals(newShoot) && !pos.equals(newShoot.copy(isTouched = true)))) {//We f is not already shoot
          return newShoot //If it is not we will use it to shoot this turn else we will try to find another
        } else {
          val newShoot2 = previousTouchShoot.copy(axisX = previousTouchShoot.axisX - 1, isTouched = false)//Try to go to the left of the previous position touched
          if (newShoot2.isInGrid && myBoard.myShoots.forall(pos => !pos.equals(newShoot2) && !pos.equals(newShoot2.copy(isTouched = true)))) {//Test to know if this psoition not already shoot
            return newShoot2
          } else {
            val newShoot2 = previousTouchShoot.copy(axisY = previousTouchShoot.axisY - 1, isTouched = false)//Try to go on the up cell of the previous touched position
            if (newShoot2.isInGrid && myBoard.myShoots.forall(pos => !pos.equals(newShoot2) && !pos.equals(newShoot2.copy(isTouched = true)))) {
              return newShoot2
            } else {
              val newShoot2 = previousTouchShoot.copy(axisY = previousTouchShoot.axisY + 1, isTouched = false) //Try to go on the down cell of the previous touched position
              if (newShoot2.isInGrid && myBoard.myShoots.forall(pos => !pos.equals(newShoot2) && !pos.equals(newShoot2.copy(isTouched = true)))) {
                return newShoot2
              } else {//If we don't find a position we will render a random shoot position and if this position is already in the shoot list we will call this method to try find another solution
                val shootPosition = Position(randX.nextInt(10), randY.nextInt(10), false)
                if (myBoard.myShoots.forall(pos => !pos.equals(shootPosition))) {
                  return shootPosition
                } else {
                  return shootPositionHard(randX,randY)
                }
              }
            }
          }
        }
    } else {
      val shootPosition = Position(randX.nextInt(10), randY.nextInt(10), false)
      return shootPosition

    }
  }

  /**
    * Method for the IA. Allow to get the good shoot function to call by the  name of the IA
    * @param randX : Random
    * @param randY : Random
    * @return : Option[Position] : If the player who call this method is not human (impossible) then we return None else we return Some(Position)
    *         which represents the shoot of the IA
    */
  def getShootFromPlayer(randX: Random, randY: Random): Option[Position] = {
    if(!isHuman) {
      if (name == "AI-easy") {
        return Some(shootPositionEasy(randX, randY))
      } else if (name == "AI-medium") {
        return Some(shootPositionMedium(randX, randY))
      } else if (name == "AI-hard") {
        return Some(shootPositionHard(randX, randY))
      } else {
        return None
      }
    }else{
      None
    }
  }
}
