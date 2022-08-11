package com.kanshu.functionalprogramming.sorting

object SelectionSortApp extends App{

  /**
   * Selection Sort
   * i/p - 5,1,4,2,8
   * o/p - 1,2,4,5,8
   */

  def minimum[T](xs: Seq[T])(implicit env: T => Ordered[T]): T = {
    if (xs.length == 1) xs.head
    else xs.tail.foldLeft(xs.head)((x,v) => if (v < x) v else x)
  }

  def selectionSort[T](xs: Seq[T], acc:Seq[T] = Nil)(implicit env: T => Ordered[T]): Seq[T] = {
    (xs,acc) match {
      case (Nil,Nil) => acc
      case (Nil, ac) => ac
      case (x :: Nil, Nil) => acc :+ x
      case (y::ys, Nil) => selectionSort(ys, acc :+ y)
      case (y::ys, ac) =>
        val rest = y::ys
        val min = minimum(rest)
        if(ac.last > min) {
          val newRest = rest.filterNot(_ == min)
          val newAcc = ac.filterNot(_ == ac.last)
          selectionSort(newRest :+ ac.last, newAcc:+ min)
        } else {
          val newRest = rest.filterNot(_ == min)
          selectionSort(newRest , ac:+ min)
        }
    }
  }

  val myList = Seq(5,1,4,2,8)
  println(selectionSort(myList))

}
