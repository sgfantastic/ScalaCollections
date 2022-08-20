package com.kanshu.functionalprogramming.sorting

object QuickSort extends App {
  /**
   * Quick Sort
   * i/p - Seq(9,5,7,6,5,3,4,2,1)
   * o/p - Seq(1,2,3,4,5,5,6,7,9)
   */

  def quickSort[V](seq: Seq[V])(implicit env: V => Ordered[V]): Seq[V] = {
    if (seq.length <= 1) seq
    else {
      val pivot = seq(seq.length/2)
      quickSort(seq.filter(pivot >)) ++
        seq.filter(pivot ==) ++
        quickSort(seq.filter(pivot <))
    }
  }

  val mySeq = Seq(9,5,7,6,5,3,4,2,1)
  println(quickSort(mySeq))


}
