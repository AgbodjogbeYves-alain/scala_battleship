package battleship

/**

**/
case class BBoard(shipList : List[Ship], opponentShoots: List[Position], myShoots: List[Position]){
  def isItTouched(position: Position) : Option[Int] = {
      //Checker si la position est dans un bateau et si oui verifier si la position a le statut touché
      
      if(position != null && !shipList.isEmpty){
          val index = shipList.indexWhere(ship => (ship.checkPositionMatch(position).get == true))
          Some(index)
      }else{
          None
      }
  }

  def aNewOpponentShoot(position: Position) : Option[BBoard] = {
    //If the shoot touch me a boat so i return a new BBoard with new shoot in opponentShoots and a new shipLiist with the position of the ship touched
    //else i just return a new BBoard with this shipList and a new myShoots
    if(position != null){
        val newOpponentShoot = position :: opponentShoots  
        if(isItTouched(position).get != -1){
            val (untouched,touched) = shipList.splitAt(isItTouched(position).get)
            val touchShip = touched.head.destroyPosition(position).get

            val newShipList = touchShip :: untouched ++ touched.tail
            
            val newBBoard = copy(shipList = newShipList,opponentShoots = newOpponentShoot)
            return Some(newBBoard)
        }else{
            val newBBoard = copy(opponentShoots = newOpponentShoot)
            return Some(newBBoard)
        }
    }else{
        None
    }
  }
  
  def myNewShoot(position: Position) : Option[BBoard] = {
    if(position != null){
        val newMyShoot =  position :: myShoots
        val newBBoard = copy(myShoots = newMyShoot)
        return Some(newBBoard)
    }else{
        return None
    }
 }

 def checkIfPositionAlreadyExist(position: Position) : Option[Int] = {
     if(position != null && !shipList.isEmpty){
          val index = shipList.indexWhere(ship => (ship.checkPositionMatch(position).get == true))
          if(index != -1){
              Some(index)
          }else{
              None
          }
      }else{
          None
      }
 } 

 def addNewShip(ship : Ship) : BBoard = {
     val newShipList = ship :: shipList
     val newBBoard = copy(shipList = newShipList)
     return newBBoard

 }

 def noShipLeft(): Option[Boolean] = {
     if(!shipList.isEmpty){
         return Some(shipList.forall(ship => ship.isSunk == true))
     }else 
        None
 }
}