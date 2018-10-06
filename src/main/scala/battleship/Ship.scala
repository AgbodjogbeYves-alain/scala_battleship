package battleship

/**
  * Class Ship
  * @param name : String : Ship name (Destroyer, Carrier , etc ...)
  * @param size : Int : Ship size
  * @param isSunk : Boolean : Define if a ship is destroy --> All is position are touched
  * @param positionList : List[Position] : Collection of position of the ship
  */
case class Ship(name: String, size: Int, isSunk: Boolean, positionList: List[Position]){
  /**
    * Check if the position in paremeter is in then ship position list
    * @param position : A position
    * @return : Option[Boolean] : Some(True) if the position is in the position collection else Some(false). If the position is Null or
    *           the positionList is empty return None
    *
    */
  def checkPositionMatch(position: Position) : Option[Boolean] = {
    if(positionList.nonEmpty && position!=null){
      val thePosTouched = position.copy(isTouched = true)
      val response = positionList.filter(pos=>{ //Check if the position is in the grid. In the position list means that the position isTouched attribute can be touched or not
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
    * Method to destroy a position and a ship if all the position are touched
    * @param position : The shoot of the opponent
    * @return : Option[Ship] : A new Option[Ship] with a new position in ship set isTouched at rrue if the position is touched else None
    *
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