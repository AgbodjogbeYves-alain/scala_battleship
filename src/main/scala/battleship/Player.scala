package battleship
import scala.annotation.tailrec
import scala.util.Random

/**
  *
  * @param name
  * @param myBoard
  */
class Player(name: String,myBoard: BBoard){

  /**
    *
    * @param position
    * @return
    */
    def receiveAShoot(position: Position) : Player = {
        val player = copy(myBoard = myBoard.aNewOpponentShoot(position).get)
        return player
        //Handle the fact that the opponent shoot you. If the player touch me i return a new player with the position add to opponentsShoot
        //and position in the ship pass to touch else a none 
    }

  /**
    *
    * @param position
    * @return
    */
    def makeAShoot(position: Position) : Player = {
        val player = copy(myBoard = myBoard.myNewShoot(position).get)
        return player
    }

  /**
    *
    * @param name
    * @param position
    * @param direction
    * @param size
    * @return
    */
    def createShip(name: String, position: Position, direction: String, size: Int) : Option[Player] = {

        val listPosition = List()
        
        if(myBoard.checkIfPositionAlreadyExist(position).get != -1 || !position.isInGrid){
            return None
        }else{
            @tailrec
            def createShipRec(name: String, position: Position,listPosition: List[Position],direction: String,size: Int) : Option[Player] = {
                if(size==0){
                    val newShip = Ship(name, listPosition.size,false,listPosition)
                    val newBBoard = myBoard.addNewShip(newShip)
                    val newPlayer = copy(myBoard = newBBoard)
                    return Some(newPlayer)
                }else{
                    if(direction == "V"){
                        val newPosition = Position(position.axisX, position.axisY+size-1, false)
                        if(myBoard.checkIfPositionAlreadyExist(newPosition).get == -1 && newPosition.isInGrid){
                            val newListPosition = newPosition :: listPosition
                            createShipRec(name,position,newListPosition,"V",size-1)
                        }else{
                            return None
                        }
                    }

                    else{
                        val newPosition = Position(position.axisX+size-1, position.axisY, false) 
                        if(myBoard.checkIfPositionAlreadyExist(newPosition).get == -1 && newPosition.isInGrid){
                            val newListPosition = newPosition :: listPosition
                            createShipRec(name,position,newListPosition,"H",size-1)
                        }else{
                            return None
                        }

                    }
                    
                }
            }
            
            createShipRec(name,position,listPosition,direction,size)
        
        }

        
    }

  /**
    *
    * @return
    */
    def stillInGame() : Boolean = {
        return !myBoard.noShipLeft.get
    }

  /**
    *
    * @return
    */
    def createMyGridForShow(): List[List[String]] = {
        val grid = List.fill(10)(List.fill(10)("N/A"))
        
        @tailrec
        def createMyGridForShowTailRec(grid: List[List[String]],shipList: List[Ship],remainShip: Int) : List[List[String]] ={
           if(remainShip<0){
               return grid
           }else{
                val showedShipPositions = shipList(remainShip).positionList
                val newGrid = defineNewGrid(showedShipPositions,grid)//Grille avec les position touchées des bateaux et les position des bateaux
                val finalGrid = addOpponentsShootsGrid(myBoard.opponentShoots,newGrid)//Grille avec les positions des bateau,des tir manqué et des tir non manqués
                createMyGridForShowTailRec(finalGrid,myBoard.shipList,remainShip-1)
            }
        }

        createMyGridForShowTailRec(grid,myBoard.shipList,myBoard.shipList.size-1)

    }

  /**
    *
    * @param positionList
    * @param grid
    * @return
    */
  def defineNewGrid(positionList: List[Position], grid : List[List[String]]) : List[List[String]] = {
    /**
      *
      * @param gridRec
      * @param remainsPosition
      * @return
      */
       @tailrec
       def defineNewGridRec(gridRec: List[List[String]] ,remainsPosition: Int) : List[List[String]] = {
           if(remainsPosition<0){
                return gridRec
            }else{
                val position = positionList(remainsPosition)
                val axisYList = gridRec(position.axisY)
                if(position.isTouched){
                    val newAxisYList = axisYList.updated(position.axisX, "-1")
                    val newGrid = gridRec.updated(position.axisY,newAxisYList)
                    defineNewGridRec(newGrid,remainsPosition-1)
                }else{
                    val newAxisYList = axisYList.updated(position.axisX, ""+positionList.size)
                    val newGrid = gridRec.updated(position.axisY,newAxisYList)
                    defineNewGridRec(newGrid,remainsPosition-1)
                }
            }
       }

       defineNewGridRec(grid,positionList.size-1)
    }

  /**
    *
    * @return
    */
    def createMyShootGridForShow(): List[List[String]] = {
        val grid = List.fill(10)(List.fill(10)("N/A"))
        
        @tailrec
        def createMyShootGridForShowRec(grid: List[List[String]], shootListSize: Int) : List[List[String]] = {
            if(shootListSize<0){
                return grid
            }else{
                val position = myBoard.myShoots(shootListSize)
                val axisYList = grid(position.axisY)
                if(position.isTouched){
                    val newAxisYList = axisYList.updated(position.axisX, "-1")
                    val newGrid = grid.updated(position.axisY,newAxisYList)
                    createMyShootGridForShowRec(newGrid,shootListSize-1)
                }else{
                    val newAxisYList = axisYList.updated(position.axisX, "1")
                    val newGrid = grid.updated(position.axisY,newAxisYList)
                    createMyShootGridForShowRec(newGrid,shootListSize-1)
                }
            }
        }
        
        createMyShootGridForShowRec(grid,myBoard.myShoots.size-1)

    }

  /**
    *
    * @param positionList
    * @param grid
    * @return
    */
    def addOpponentsShootsGrid(positionList: List[Position], grid : List[List[String]]) : List[List[String]] = {

       @tailrec
       def addOpponentsShootsGridRec(gridRec: List[List[String]] ,remainsPosition: Int) : List[List[String]] = {
           if(remainsPosition<0){
                return gridRec
            }else{
                val position = positionList(remainsPosition)
                val axisYList = gridRec(position.axisY)
                if(!position.isTouched){
                    val newAxisYList = axisYList.updated(position.axisX, "MO")
                    val newGrid = gridRec.updated(position.axisY,newAxisYList)
                    addOpponentsShootsGridRec(newGrid,remainsPosition-1)
                }else{
                    addOpponentsShootsGridRec(gridRec,remainsPosition-1)
                }
            }
       }

       addOpponentsShootsGridRec(grid,positionList.size-1)
    }

  /**
    *
    * @param randX
    * @param randY
    * @return
    */
    def generateRandomPosition(randX: Random, randY: Random) : Position = {
        val axisX = randX.nextInt(10)
        val axisY = randY.nextInt(10)
        val generatedPosition = Position(axisX,axisY,false)
        return generatedPosition

    }

  /**
    *
    * @param randDir
    * @return
    */
    def generateRandomDirection(randDir: Random) : String = {
        val dir = randDir.nextInt
        if(dir%2 == 0){
            return "V"
        }else{
            return "H"
        }
    }

  /**
    *
    * @param randX
    * @param randY
    * @return
    */
    def shootPosition(randX: Random, randY: Random) : Option[Position] = {
        if(name == "AI-easy"){
            return Some(Position(randX.nextInt(10),randY.nextInt(10),false))
        }else{
            None
        }
    }

  def
}
    