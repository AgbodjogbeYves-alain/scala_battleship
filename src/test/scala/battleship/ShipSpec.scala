package battleship

import org.scalatest._

class ShipSpec extends FlatSpec with Matchers {
    val position1 = Position(1,1,false)
    val position2 = Position(10,1,false)
    val position3 = Position(1,10,false)
    val position4 = Position(1,1,false)
    val position5 = Position(1,1,true)

    val positionList = List(position1,position2,position3)
    val ship = Ship("US",3,false,positionList)

    val newListWithDestroyPosition = List(position5,position2,position3)
    val shipDestroy = ship.copy(positionList = newListWithDestroyPosition)

    "The Ship Object" should "show values" in {
        ship.name should be ("US")
        ship.size should be (3)
        ship.isSunk should be (false)
        ship.positionList should be (positionList)
        ship.checkPositionMatch(position4) should be (Some(true))
        ship.destroyPosition(position4).get should be (shipDestroy)
    }

    val positionD1 = Position(1,1,true)
    val positionD2 = Position(10,1,true)
    val positionD3 = Position(1,10,false)
    val positionD4 = Position(1,10,false)
    val positionD5 = Position(1,10,true)

    val positionDList = List(positionD1,positionD2,positionD3)
    val shipD = Ship("USD",3,false,positionDList)

    "The destroy ship Object" should "show values" in {
        shipD.destroyPosition(positionD4).get.positionList should be (List(positionD5,positionD1,positionD2))
        shipD.destroyPosition(positionD4).get.isSunk should be (true)
    }
}
