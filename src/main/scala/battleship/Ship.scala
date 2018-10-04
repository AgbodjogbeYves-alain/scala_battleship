package battleship

/**
  *
  * @param name
  * @param size
  * @param isSunk
  * @param positionList
  */
case class Ship(name: String, size: Int, isSunk: Boolean, positionList: List[Position]){
  /**
    *
    * @param position
    * @return
    */
  def checkPositionMatch(position: Position) : Option[Boolean] = {
    if(positionList!=null && position!=null){
      val thePosTouched = position.copy(isTouched = true)
      val response = positionList.filter(pos=>{
        pos.equals(thePosTouched) || pos.equals(position)
      })
      if(response.size>=1){
        Some(true)
      }else{
        Some(false)
      }
    }
    else
      None  
  }

  /**
    *
    * @param position
    * @return
    */
  def destroyPosition(position: Position) : Option[Ship] = {
    //If the shoot match a position of this boat then i return a new boat copy of this with the new position
    //else i return none
    checkPositionMatch(position) match{
      case Some(true) => {
                          val newPosition = Position(position.axisX,position.axisY,true)
                          val newPositionList = newPosition :: positionList.filter(pos => !pos.equals(position) )
                          val newIsSunk = newPositionList.forall(pos => (pos.isTouched == true))
                          val newShip = copy(isSunk = newIsSunk, positionList = newPositionList)
                          return Some(newShip)
      }
      case _ => None
    }
  }
}