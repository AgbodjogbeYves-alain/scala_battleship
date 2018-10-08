package battleship

/**
  * Class which represents the battle board of the player
  * @param shipList : List[Ship] : Boat list of the player
  * @param opponentShoots : List[Position] : Shoot from the opponents on the player board list of the player
  * @param myShoots : List[Position] : Player shoot on the opponetn board
  */
case class BBoard(shipList : List[Ship], opponentShoots: List[Position], myShoots: List[Position]){
  /**
    * Method to check if the position in parameter is one of the player ship posiiton
    * @param position : Position : The position to check
    * @return : Option[Int] : -1 if the position does not exist, Some(Int) if the position exist and the int represent the index of the ship else None
    */
  def isItTouched(position: Position) : Option[Int] = {
      if(position != null && !shipList.isEmpty){
          val index = shipList.indexWhere(ship => (ship.checkPositionMatch(position).get == true))
          Some(index)
      }else{
          None
      }
  }

  /**
    * Mehtod to handle a new shoot from the opponent
    * @param position :Position : Shoot from the opponent
    */
  def aNewOpponentShoot(position: Position) : Option[BBoard] = {
    //If the shoot touch me a boat so i return a new BBoard with new shoot in opponentShoots and a new shipLiist with the position of the ship touched
    //else i just return a new BBoard with this shipList and a new myShoots
    if(position != null){
        if(isItTouched(position).get != -1){
            val newPosition = position.copy(isTouched = true)
            val newOpponentShoot = newPosition :: opponentShoots

            val (untouched,touched) = shipList.splitAt(isItTouched(position).get)
            val touchShip = touched.head.destroyPosition(position).get

            val newShipList = touchShip :: untouched ++ touched.tail
            
            val newBBoard = copy(shipList = newShipList,opponentShoots = newOpponentShoot)
            return Some(newBBoard)
        }else{
          val newOpponentShoot = position :: opponentShoots
          val newBBoard = copy(opponentShoots = newOpponentShoot)
            return Some(newBBoard)
        }
    }else{
        None
    }
  }

  /**
    * Method to registered player new shoot on the opponent board
    * @param position : Position : The new shoot
    * @return : Option[BBoard] : Return a new board with the shoot added to myShoot list
    */
  def myNewShoot(position: Position) : Option[BBoard] = {
    if(position != null){
        val newMyShoot =  position :: myShoots
        val newBBoard = copy(myShoots = newMyShoot)
        return Some(newBBoard)
    }else{
        return None
    }
 }

  /**
    * Method to check if a position already exist in the board
    * @param position : Position : The position to check
    * @return : Option[Int] : -1 if the position does not exist, Some(Int) if the position exist and the int represent the index of the ship else None
    */
 def checkIfPositionAlreadyExist(position: Position) : Option[Int] = {
     if(position != null){
          val index = shipList.indexWhere(ship => (ship.checkPositionMatch(position).get == true))
          Some(index)
      }else{
          None
      }
 }

  /**
    * Method to add a new ship in the player board
    * @param ship : Ship : The ship to add
    * @return : Option[BBoard] : Return a new board with the ship added to shipList
    */
 def addNewShip(ship : Ship) : BBoard = {
     val newShipList = ship :: shipList
     val newBBoard = copy(shipList = newShipList)
     return newBBoard

 }

  /**
    * Method to check if all the ships are sunked
    * @return : Option[Boolean] : Return Some(true) if all the ship are sunked,return Some(false) if at least 1 ship is not sunked else if the ship list is empty return None
    */
 def noShipLeft(): Option[Boolean] = {
     if(!shipList.isEmpty){
         return Some(shipList.forall(ship => ship.isSunk == true))
     }else 
        None
 }
}