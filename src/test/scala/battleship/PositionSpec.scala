package battleship

import org.scalatest._

class PositionSpec extends FlatSpec with Matchers {
    val position1 = Position(1,1,true)
    val position2 = Position(20,1,false)
    val position3 = Position(1,20,false)


    "The Position 1 Object" should "show values" in {
        position1.axisX  should be (1)
        position1.axisY  should be (1)
        position1.isTouched should be (true)
        position1.isInGrid should be (true)
    }

    "The Position 2 Object" should "show values" in {
        position2.isInGrid should be (false)
        position2.isTouched should be (false)
    }

    "The Position 3 Object" should "show values" in {
        position3.isInGrid should be (false)

    }
    
}
