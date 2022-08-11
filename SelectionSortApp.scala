package com.kanshu.functionalprogramming.sorting

object SelectionSortApp extends App {

  /**
   * Selection Sort
   * i/p - 5,1,4,2,8
   * o/p - 1,2,4,5,8
   */

  def minimum[T](xs: Seq[T])(implicit env: T => Ordered[T]): T ={
    if (xs.length == 1) xs.head
    else xs.tail.foldLeft(xs.head)((x,v) => if (v < x) v else x)
  }

  def selectionSort[T](unsorted:Seq[T], sorted: Seq[T] = Nil)
                      (implicit env:T => Ordered[T]): Seq[T] ={
    (unsorted, sorted) match {
      case (Nil, Nil) => Nil
      case (Nil, ac) => ac
      case (x :: Nil, ac) => ac :+ x
      case (y :: ys, Nil) => selectionSort(ys, sorted :+ y)
      case (rest, ac) =>
        val min = minimum(rest)
        if(ac.last > min) {
          val newRest = rest.filterNot(_ == min)
          val newSorted = ac.filterNot(_ == ac.last)
          selectionSort(newRest :+ ac.last, newSorted :+ min)
        } else {
          val newRest = rest.filterNot(_ == min)
          selectionSort(newRest, ac :+ min)
        }
    }
  }

  val myList = Seq(5,1,4,2,8)
  println(selectionSort(myList))

}
