package battleship

/**
  * Class position.
  * @param axisX : Int : Position on the grid in X
  * @param axisY : Int : Position on the grid in Y
  * @param isTouched : Boolean : Represent if the position is destroy by a opponent shoot
  */
case class Position(axisX : Int, axisY : Int, isTouched: Boolean ){
  /**
    * Method to verify if a position is in the grid which means that his x and y position are less than 10
    * @return Boolean : True if is in else false
    */
  def isInGrid : Boolean = axisX < 10 && axisY < 10 && axisX>=0 && axisY>=0 
}

