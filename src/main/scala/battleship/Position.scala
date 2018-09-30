package battleship

case class Position(axisX : Int, axisY : Int, isTouched: Boolean ){
  def isInGrid : Boolean = axisX <= 10 && axisY <= 10 
}

