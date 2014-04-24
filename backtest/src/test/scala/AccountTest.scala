import org.scalatest.FunSuite
import org.scalatest.Matchers._

class AccountTest extends FunSuite {
  test("equity equals balance on start") {
    val acc = Account(BigDecimal("100.0"), leverage = 100)
    BigDecimal("100.0") should equal(acc.equity)
  }
  test("registerProfit") {
    val acc = Account(BigDecimal("100.0"), leverage = 100)
    acc.realiseFloating(5F)
    105F shouldEqual(acc.balance)
  }
}
