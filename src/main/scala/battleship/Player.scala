package battleship
import scala.annotation.tailrec

case class Player(name: String,myBoard: BBoard){
    def receiveAShoot(position: Position) : Player = {
        val player = copy(myBoard = myBoard.aNewOpponentShoot(position).get)
        return player
        //Handle the fact that the opponent shoot you. If the player touch me i return a new player with the position add to opponentsShoot
        //and position in the ship pass to touch else a none 
    }

    def makeAShoot(position: Position) : Player = {
        val player = copy(myBoard = myBoard.myNewShoot(position).get)
        return player
    }

    
    def createShip(name: String, position: Position, direction: String, size: Int) : Option[Player] = {

        val listPosition = List()
        
        @tailrec
        def createShipRec(name: String, position: Position,listPosition: List[Position],direction: String,size: Int) : Option[Player] = {
            if(size==0){
                val newShip = Ship(name, listPosition.size,false,listPosition)
                val newBBoard = myBoard.addNewShip(newShip)
                val newPlayer = copy(myBoard = newBBoard)
                return Some(newPlayer)
            }else{
                if(direction == "V"){
                    val newPosition = Position(position.axisX, position.axisY+size, false)
                    if(myBoard.checkIfPositionAlreadyExist(newPosition) == -1 && newPosition.isInGrid){
                        val newListPosition = newPosition :: listPosition
                        createShipRec(name,position,newListPosition,"V",size-1)
                    }else{
                        return None
                    }
                }

                else{
                    val newPosition = Position(position.axisX+size, position.axisY, false) 
                    if(myBoard.checkIfPositionAlreadyExist(newPosition) == -1 && newPosition.isInGrid){
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

    def stillInGame() : Boolean = {
        return !myBoard.noShipLeft.get
    }

    def createMyGridForShow(): List[List[String]] = {
        val grid = List.fill(10)(List.fill(10)("N/A"))
        
        @tailrec
        def createMyGridForShowTailRec(grid: List[List[String]],shipList: List[Ship],remainShip: Int) : List[List[String]] ={
           if(remainShip==0){
               return grid
           }else{

                createMyGridForShowTailRec(grid,myBoard.shipList,remainShip-1)
           }

        }

        createMyGridForShowTailRec(grid,myBoard.shipList,myBoard.shipList.size)
    }

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
                    val newAxisYList = axisYList.updated(position.axisX, Console.WHITE)
                    val newGrid = grid.updated(position.axisY,newAxisYList)
                    createMyShootGridForShowRec(newGrid,shootListSize-1)
                }else{
                    val newAxisYList = axisYList.updated(position.axisX, Console.RED)
                    val newGrid = grid.updated(position.axisY,newAxisYList)
                    createMyShootGridForShowRec(newGrid,shootListSize-1)
                }
            }
        }
        
        createMyShootGridForShowRec(grid,myBoard.myShoots.size-1)

    }
}
    