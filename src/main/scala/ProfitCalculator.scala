import scala.annotation.tailrec

/*
  Profit calculator to find the best day to buy and sell goods in order to get the highest profit.
  It is taken that we know data for X days in advance.
  In addition we can only sell good only the next day, but not the same day as it was purchased.
*/
object ProfitCalculator extends App {
  def getBuyDay(dailyPrices: List[PurchaseDay]) : Int = {
    getMaxProfitDay(dailyPrices).purchaseDay
  }

  def getSellDay(dailyPrices: List[PurchaseDay]): Int = {
    getMaxProfitDay(dailyPrices).sellDay
  }

  def getMaxProfitDay(dailyPricing: List[PurchaseDay]): CountedMaxProfitDay = {
    countDailyProfitability(dailyPricing.sortBy(dailyPrice => dailyPrice.price), List.empty).maxBy(_.profit)
  }

  @tailrec
  /*
    It is not optimal to go through the list over and over again, especially if the amount of data is big.
    I am sure there must be better searching algorithm that would be possible to implement using tail recursion,
    but I should have relied on what I know so far, so let it be :)
  */
  def countDailyProfitability(sortedByPriceDailyPricing: List[PurchaseDay], calculatedDailyProfit: List[CountedMaxProfitDay]): List[CountedMaxProfitDay] = {
    if (sortedByPriceDailyPricing.length < 2) calculatedDailyProfit
    else {
      val minPriceDay = sortedByPriceDailyPricing.head
      val dailyProfit = sortedByPriceDailyPricing.filter(dailyPrice => dailyPrice.purchaseDay > minPriceDay.purchaseDay )
        .map (dailyPrice =>
          CountedMaxProfitDay(minPriceDay.purchaseDay, dailyPrice.purchaseDay, dailyPrice.price - minPriceDay.price)
        )

      countDailyProfitability(sortedByPriceDailyPricing.takeRight(sortedByPriceDailyPricing.length - 1), calculatedDailyProfit ++ dailyProfit)
    }
  }

  /*
    Note, this one uses ordinary recursion, which will cause StackOverflow if
    it is needed to traverse the tree almost till the leaf nodes.
    This is due to limited memory resources allocated in Scala for storing each result separately.
  */
  def findMaxProfitDay(dailyPricing: List[PurchaseDay]): CountedMaxProfitDay = {
    val maxPriceDay = dailyPricing.maxBy(_.price)
    val minPriceDay = dailyPricing.minBy(_.price)
    val maxProfitDays = CountedMaxProfitDay(minPriceDay.purchaseDay, maxPriceDay.purchaseDay, maxPriceDay.price - minPriceDay.price)

    if (maxProfitDays.sellDay > maxProfitDays.purchaseDay) maxProfitDays
    else if (dailyPricing.length <= 2) CountedMaxProfitDay(minPriceDay.purchaseDay, maxPriceDay.purchaseDay, maxPriceDay.price - minPriceDay.price)
    else {
      val rightMaxProfit = findMaxProfitDay(dailyPricing.take(maxPriceDay.purchaseDay))
      val leftMaxProfit = findMaxProfitDay(dailyPricing.drop(maxPriceDay.purchaseDay))

      Set(rightMaxProfit, leftMaxProfit).maxBy(_.profit)
    }
  }
}

final case class CountedMaxProfitDay(purchaseDay: Int, sellDay: Int, profit: Int)
final case class PurchaseDay(purchaseDay: Int, price: Int)
