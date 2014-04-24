import org.scalatest.FunSuite
import org.scalatest.Matchers._

class TimeSeriesTest extends FunSuite {
  test("new TimeSeries is empty") {
    val ts = new TimeSeries(20)
    assert(ts.size == 0)
  }

  test("ts with 100 quotes added has proper size") {
    val ts = new TimeSeries(5)
    (1 to 111).foreach {
      _ => ts.add(1)
    }
    assert(ts.size == 5)
  }

  test("ts with many quotes tail is valid") {
    val ts = new TimeSeries(5)
    (1 to 50).foreach {
      i => ts.add(i)
    }
    val res = ts.getLast(5)
    res should equal (Array(46, 47, 48, 49, 50))
  }

  test("moving average") {
    val ts = new TimeSeries(101)
    (0 to 100).foreach {
      i => ts.add(i)
    }
    val res = MA.calc(ts.getLast())
    res should equal (50)
  }

}
