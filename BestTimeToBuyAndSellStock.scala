package com.kanshu.datastructures

object BestTimeToBuyAndSellStock extends App{
  /**
   * Given an array prices[] of length N, representing the prices of the stocks on different days,
   * the task is to find the maximum profit possible for buying and selling the stocks
   * on different days using transactions where at most one transaction is allowed.
   * input prices = [7, 1, 5, 3, 6, 4]
   * output = 5
   * - on day 2 the price is lowest - [_, 1 , _ , _ , _,_]
   * - on day 5 the price is highest - [_, 1 , _ , _ , 6,_]
   * - profit = 6-1 = 5
   */
// for one buy and sell calculate max profit
  private val prices = Seq(7,1,5,2,6,4)

  private def maxProfit(prices: Seq[Int], buy: Int, mxProfit: Int = 0): Int ={
    prices match {
      case Nil => mxProfit
      case price :: priceLst =>
        (buy, mxProfit) match {
          case (b,mx) if b > price => maxProfit(priceLst,price,mx)
          case (b,mx) if price-b > mx => maxProfit(priceLst, b, price-b)
          case _ => maxProfit(priceLst, buy, mxProfit)
        }
    }
  }

  println(maxProfit(prices,prices.head))

  private val priceZip = prices.zipWithIndex

  private def maxProfitPos(prices: Seq[(Int, Int)],
                           buy: Int,
                           mxProfit: Int = 0,
                           position: (Int, Int)= (0,0)): (Int, (Int,Int)) ={
    prices match {
      case Nil => (mxProfit, position)
      case price :: priceLst =>
        println(s"$prices -- $buy -- $mxProfit -- $position")
        (buy, mxProfit, position) match {
          case (b,mx, pos) if b > price._1 && price._2 > pos._1 =>
            pos match {
              case p if p._2 == 0 => maxProfitPos(priceLst, price._1, mx, (price._1,p._2))
              case p if price._2 > p._2 =>maxProfitPos(priceLst, b,mx,p)
              case _ => maxProfitPos(priceLst, b,mx,pos)
            }
          case (b,mx,pos) if price._1 -b > mx =>
            pos match {
              case p if price._2 < p._1 => maxProfitPos(priceLst,b,mx,p)
              case _ => maxProfitPos(priceLst, b , price._1-b, (pos._1, price._2))
            }
          case _ => maxProfitPos(priceLst, buy, mxProfit, position)
        }
    }
  }

  println(maxProfitPos(priceZip, priceZip.head._1))







}
