package battleship

import org.scalatest._

class PlayerSpec extends FlatSpec with Matchers {


    //Val for createMyShootGridForShow Test 1 &&  //Val for createMyGridForShow Test 1

    val position1 = Position(0,0,false)
    val position2 = Position(1,0,false)
    val position3 = Position(2,0,false)
    val position4 = Position(5,3,false)


    val positionList = List(position1,position2,position3,position4)
    val ship = Ship("US",4,false,positionList)

    val positionD1 = Position(0,0,true)
    val positionD2 = Position(1,0,true)
    val positionD3 = Position(2,0,false)
    val positionD4 = Position(5,5,false)
    val positionD5 = Position(6,5,false)


    val positionEnd = Position(6,5,true)
    val shipEnd = Ship("End",1,true,List(positionEnd))
    val boardEnd = BBoard(List(shipEnd),List(),myShoots)
    val playerEnd = Player("Yves",boardEnd,true)

    val myShoots = List(positionD1,positionD2,positionD3)

    val myBBoard = BBoard(List(ship),List(),myShoots)
    val player = Player("Submarine",myBBoard,true)

    //Val for create ship Test 1
    val newPos1H = Position(5,5,false)
    val newPos2H = Position(6,5,false)

    val newPos1V = Position(5,5,false)
    val newPos2V = Position(5,6,false)

    val newPos1V2 = Position(6,5,false)
    val newPos2V2 = Position(6,6,false)

    val newShipH = Ship("Carrier",2,false,List(newPos1H,newPos2H))
    val newMyBoardH = myBBoard.copy(shipList = newShipH :: List(ship))
    val newPlayerH = player.copy(myBoard = newMyBoardH)

    val newShipV = Ship("Carrier",2,false,List(newPos1V,newPos2V))
    val newMyBoardV = myBBoard.copy(shipList = newShipV :: List(ship))
    val newPlayerV = player.copy(myBoard = newMyBoardV)

    val newShipV2 = Ship("Carrier",2,false,List(newPos1V2,newPos2V2))
    val newMyBoardV2 = myBBoard.copy(shipList = newShipV2 :: List(newShipV,ship))
    val newPlayerV2 = player.copy(myBoard = newMyBoardV2)

    "The player object" should "valid its infos" in {
      player.name should be ("Submarine")
    }

    //All tests for player
    "The Player Object" should "valid receiveAShoot" in {
        player.receiveAShoot(positionD2) should be (player.copy(myBoard = myBBoard.aNewOpponentShoot(positionD2).get))
        player.receiveAShoot(positionD4) should be (player.copy(myBoard = myBBoard.aNewOpponentShoot(positionD4).get))

    }

    "The Player Object" should "valid makeAShoot" in {
        player.makeAShoot(positionD2) should be (player.copy(myBoard = myBBoard.myNewShoot(positionD2).get))
        player.makeAShoot(positionD4) should be (player.copy(myBoard = myBBoard.myNewShoot(positionD4).get))
    }


    "The Player Object" should "valid createShip" in {
        player.createShip("Carrier", positionD4,"H",2).get should be (newPlayerH)
        player.createShip("Carrier", position1,"H",2) should be (None)
        player.createShip("Carrier", positionD4,"V",2).get should be (newPlayerV)
        player.createShip("Carrier", positionD4,"V",2).get.createShip("Carrier", positionD5,"V",2).get should be (newPlayerV2)



    }
    

    "The Player Object" should "valid createMyShootGridForShow" in {
        player.createMyShootGridForShow() should be (List(List("-1","-1","1","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A")))
    }

    "The Player Object" should "valid createMyGridForShow" in {
        player.createMyGridForShow() should be (List(List("4","4","4","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","4","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A"),List("N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A","N/A")))
    }

    "The Player Object" should "valid stillInGame" in {
        player.stillInGame() should be (true)
        playerEnd.stillInGame() should be (false)
    }



}