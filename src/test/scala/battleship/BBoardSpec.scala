package battleship

import org.scalatest._

class BBoardSpec extends FlatSpec with Matchers {
    val position1 = Position(1,1,false)
    val position2 = Position(10,1,false)
    val position3 = Position(1,10,false)
    val position4 = Position(1,1,false)
    val position5 = Position(1,6,true)
    val position6 = Position(10,1,true)

    val positionList = List(position1,position2,position3)
    val ship = Ship("US",3,false,positionList)

    val newListWithDestroyPosition = List(position2,position3)
    val shipDestroy = ship.copy(positionList = newListWithDestroyPosition)

    val positionD1 = Position(1,1,true)
    val positionD2 = Position(10,1,true)
    val positionD3 = Position(1,10,false)
    val positionD4 = Position(1,10,false)
    val positionD5 = Position(1,10,true)

    val positionDList = List(positionD1,positionD2,positionD3)
    val shipD = Ship("USD",3,false,positionDList)

    val shootedShip = ship.copy(positionList = List(position6,position1,position3))

    val myBBoard = BBoard(List(ship,shipDestroy),List(),List())
    val myBoardModified = myBBoard.copy(shipList = List(shipD,ship,shipDestroy))
    val myBoardWithMyNewShoot = myBBoard.copy(myShoots = List(position1))
    val myBoardWithOpponentNewShoot = myBBoard.copy(shipList = List(shootedShip,shipDestroy), opponentShoots = List(position2))

    "The BBoard Object" should "valid isItTouched" in {
        myBBoard.isItTouched(position4) should be (Some(0))
        myBBoard.isItTouched(position5) should be (Some(-1))
    }

    "The BBoard Object" should "valid aNewOpponentShoot" in {
        myBBoard.aNewOpponentShoot(position2).get should be (myBoardWithOpponentNewShoot)
    }

    "The BBoard Object" should "valid myNewShoot" in {
        myBBoard.myNewShoot(position1).get should be (myBoardWithMyNewShoot)
    }

    "The BBoard Object" should "valid checkIfPositionAlreadyExist" in {
        myBBoard.checkIfPositionAlreadyExist(position1) should be (Some(0))
    }

    "The BBoard Object" should "valid addNewShip" in {
        myBBoard.addNewShip(shipD) should be (myBoardModified)

    }

    "The BBoard Object" should "valid noShipLeft" in {
        myBBoard.noShipLeft() should be (Some(false))
    }
}