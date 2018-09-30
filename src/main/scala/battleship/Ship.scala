package battleship

/**
**/
case class Ship(name: String, size: Int, isSunk: Boolean, positionList: List[Position]){
  def checkPositionMatch(position: Position) : Option[Boolean] = {
    if(positionList!=null && position!=null){
      val response = positionList.filter(pos=>pos.axisX==position.axisX && pos.axisY==position.axisY)
      if(response.size>=1){
        Some(true)
      }else{
        Some(false)
      }
    }
    else
      None  
  } 

  def destroyPosition(position: Position) : Option[Ship] = {
    //If the shoot match a position of this boat then i return a new boat copy of this with the new position
    //else i return none
    checkPositionMatch(position) match{
      case Some(true) => {
                          val newPosition = Position(position.axisX,position.axisY,true)
                          val newPositionList = newPosition :: positionList.filter(pos => !pos.equals(position) )
                          val newIsSunk = newPositionList.forall(pos => pos.isTouched == true)
                          val newShip = copy(isSunk = newIsSunk, positionList = newPositionList)
                          return Some(newShip)
      }
      case _ => None
    }
  }
}
/*object CoinFlip extends App {
 val position1 = Position(1,1,true)
 val position2 = Position(20,1,true)
 val position3 = Position(1,20,false)
 val position4 = Position(1,1,false)
 val positionList = List(position1,position2,position3)  
}*/