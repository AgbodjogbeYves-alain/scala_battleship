package battleship

case class Position(axisX : Int, axisY : Int, isTouched: Boolean ){
  /**
    *
    * @return
    */
  def isInGrid : Boolean = axisX < 10 && axisY < 10 && axisX>=0 && axisY>=0 
}

