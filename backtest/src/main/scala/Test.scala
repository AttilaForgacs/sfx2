import fx._
import scala.collection.mutable
import scala.io.Source


trait Strategy {
  def onBar(bar: Bar)
}

class TimeSeries(val lookback: Int) {
  val bufferSize = if (lookback > 50) lookback else 50
  var prices = new Array[Float](bufferSize)
  var idx = 0

  def add(price: Float) {
    prices(idx) = price
    idx += 1
    if (idx == bufferSize) {
      val savePrices = prices.takeRight(lookback)
      prices = new Array[Float](bufferSize)
      savePrices.copyToArray(prices)
      idx = lookback
    }
  }

  def getLast(n: Int = lookback): Array[Float] = {
    prices.slice(idx - n, idx)
  }

  def size: Int = Math.min(lookback, idx)
}

object MA {
  def calc(ts: Array[Float]): Float = {
    if (ts.size > 0)
      ts.sum / ts.size
    else
      0F
  }
}

sealed trait MarketSide

case object Buy extends MarketSide

case object Sell extends MarketSide

case class S1() extends Strategy {

  val ma1 = new TimeSeries(20)
  val ma2 = new TimeSeries(50)

  override def onBar(quote: Bar) {
    //    ma1.add(price)
    //    ma2.add(price)
    //    MA.calc(ma1.getLast())
    //    MA.calc(ma2.getLast())
  }
}

case class Account(deposit: BigDecimal, leverage: Int) {

  def realiseFloating(profit: Float) {
    equity -= profit
    balance += profit
  }

  var balance = cloneBigDecimal(deposit)
  var equity = cloneBigDecimal(deposit)
}

case class Position(lots: Float,
                    side: MarketSide,
                    account: Account,
                    var openPrice: Option[Float] = None,
                    var closePrice: Option[Float] = None,
                    var profit: Float = 0F) {

  def calcProfit: Float = {
    val mul: Float = lots * account.leverage
    side match {
      case Buy =>
        profit = (closePrice.get - openPrice.get) * mul
      case Sell =>
        profit = (openPrice.get - closePrice.get) * mul
    }
    profit
  }

  def update(bar: Bar) {
    closePrice = Some(bar.open)
    calcProfit
  }
}

case class VolatilityStrategy(account: Account, positions: mutable.ArrayBuffer[Position]) extends Strategy {

  var closedPositions: List[Position] = List.empty

  def updatePositions {
    positions.foreach(p => p.update(lastBar))
  }

  var lastBar: Bar = null

  def closePositions {
    for (position <- positions) {
      if (position.profit > 10) {
        closedPositions = position :: closedPositions
        positions -= position
        account.realiseFloating(position.profit)
      }
    }
  }

  override def onBar(bar: Bar) {
    lastBar = bar
    val buySignal = (bar.open - bar.close) > 0.0020
    if (buySignal)
      buySingle
    updatePositions
    closePositions
  }

  def buySingle = {
    if (positions.length == 0) {
      val p = Position(1, Buy, account, Some(lastBar.open))
      positions += p
    } else null
  }
}

case class Test(file: String, strat: Strategy) {
  val format = new java.text.SimpleDateFormat("yyyy.MM.dd HH:mm:ss")

  def run() {
    val src = Source.fromFile(file)
    val iter = src.getLines().drop(1).map(_.split(","))
    iter.foreach((row: Array[String]) => strat.onBar(getRow(row)))
    src.close()
  }

  def getRow(a: Array[String]): Bar = {
    val date = format.parse(a(0))
    val open = a(1).toFloat
    val high = a(2).toFloat
    val low = a(3).toFloat
    val close = a(4).toFloat
    val volume = a(5).toFloat
    Bar(date, open, high, low, close, volume)
  }

}

object Main {
  val file = "C:\\data\\USDCHF_UTC_1 Min_Bid_2013.01.01_2013.12.31.csv"

  def main(args: Array[String]) {
    val strat = VolatilityStrategy(
      Account(BigDecimal("10000.0"), leverage = 100),
      new mutable.ArrayBuffer[Position]()
    )
    new Test(file, strat).run
    println(strat.account.balance)

  }
}
