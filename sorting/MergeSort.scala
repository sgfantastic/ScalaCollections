package com.kanshu.sparkscala.sorting

object MergeSort extends App {

  class MergeSort[+T] {
    def mergeSort[T](xs: List[T])(implicit ev: T => Ordered[T]): List[T] ={
      def merge(xs: List[T], ys: List[T]): List[T] = {
        (xs, ys) match {
          case (Nil, _) => xs
          case (_, Nil) => xs
          case (x :: xs1, y :: ys1) =>
            if (x < y) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }
      }
      val n = xs.length/2
      if (n == 0) xs
      else {
        val (ys, zs) = xs splitAt n
        merge(mergeSort(ys),mergeSort(zs))
      }
    }
  }

  val myList: List[Int] = List(17,16,15,9,8,7,2,3,5)
  val mSort = new MergeSort[Int]
  println(mSort.mergeSort(myList))

}
