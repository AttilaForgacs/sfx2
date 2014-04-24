import org.scalatest.FunSuite
import org.scalatest.Matchers._

class PositionTest extends FunSuite {

  val account = Account(BigDecimal("100"), leverage = 100)

  test("positive profit") {
    val p = Position(1, Buy, account, Some(100F), Some(200F))
    10000 shouldEqual (p.calcProfit)
  }

  test("negative profit") {
    val p = Position(1, Buy, account, Some(200F), Some(100F))
    (-10000) shouldEqual (p.calcProfit)
  }

}