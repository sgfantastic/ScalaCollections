package com.kanshu.functionalprogramming.sorting

object BubbleSortApp extends App{
  /**
   * Bubble Sort
   *  input - 5,1,4,2,8
   *  sorted - 1,2,4,5,8
   */

  def bubbleSort[T](list: Seq[T])(implicit env: T => Ordered[T]): Seq[T] ={
    // bubble
    def bubble(as: Seq[T], zs: Seq[T], bs: Seq[T]): Seq[T] ={
      as match {
        case h1 :: h2 :: tail if h1 > h2 => bubble(h1::tail, h2+:zs, bs)
        case h1 :: h2 :: tail => bubble(h2::tail, h1+:zs, bs)
        case h1 :: Nil => sort(zs, h1 +: bs)
      }
    }
    // sort
    def sort(as: Seq[T], bs: Seq[T]): Seq[T] ={
      as match {
        case Nil => bs
        case _ => bubble(as,Nil,bs)
      }
    }

    sort(list,Nil)
  }

  val lst = Seq(5,1,4,2,8)
  println(bubbleSort(lst))

}
