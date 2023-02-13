import org.scalatest.flatspec.*
import ProfitCalculator._
import ProfitCalculatorSpec._

sealed class ProfitCalculatorSpec extends AnyFlatSpec {
  "Profit calculator" should "find correctly max profit days using tail recursion" in {
    val expectedProfit = CountedMaxProfitDay(1,2,2)
    val anotherExpectedProfit = CountedMaxProfitDay(3,4,8)
    assert(getMaxProfitDay(defaultDailyPricing) == expectedProfit)
    assert(getMaxProfitDay(anotherDailyPricing) == anotherExpectedProfit)
  }

  it should "find correctly max profit days with an ordinary recursion and simple cases" in {
    val expectedProfit = CountedMaxProfitDay(1, 2, 2)
    assert(findMaxProfitDay(defaultDailyPricing) == expectedProfit)
  }

  it should "throw StackOverflow exception for max profit days search in case of ordinary recursion and more complex search tree" in {
    assertThrows[StackOverflowError] {
      findMaxProfitDay(anotherDailyPricing)
    }
  }

  it should "return min price day correctly" in {
    assert(getBuyDay(dailyPricing)==4)
  }

  it should "return max price day correctly" in {
    assert(getSellDay(dailyPricing)==5)
  }
}

object ProfitCalculatorSpec {
  var defaultPricingDay = PurchaseDay(2, 8)
  var defaultCheaperPricingDay = PurchaseDay(1, 6)
  var defaultDailyPricing = List(defaultCheaperPricingDay, defaultPricingDay)
  var dailyPricing = List(defaultCheaperPricingDay, defaultPricingDay, PurchaseDay(3,15), PurchaseDay(4,1), PurchaseDay(5,14))
  var anotherDailyPricing = List(PurchaseDay(0,7), PurchaseDay(1,12), PurchaseDay(2,5), PurchaseDay(3,3), PurchaseDay(4,11), PurchaseDay(5,6), PurchaseDay(6,10), PurchaseDay(7,2), PurchaseDay(8,9))
}
