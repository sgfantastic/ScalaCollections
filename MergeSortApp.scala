package com.kanshu.functionalprogramming.sorting

object MergeSortApp extends App{
  /**
   *  9,7,8,3,2,1
   *  9,7,8 -- 3,2,1
   *  9,7 -- 8  3,2 -- 1
   *  9 -- 7 -- 8 -- 3 -- 2 -- 1
   *  7,9 -- 8  2,3 -- 1
   *  7,8,9 -- 1,2,3
   *  1,2,3,7,8,9
   */

  def mergeSort[V](input: Seq[V])(implicit env: V => Ordered[V]): Seq[V] = {
    def merge(left: Seq[V], right: Seq[V]): Seq[V] =
      (0 until left.length + right.length)
        .foldLeft(Seq[V](), left, right)((triple, _) => {
          val (merged, leftRemaining, rightRemaining) = triple
          (leftRemaining, rightRemaining) match {
            case (Nil, r :: rTail) => (r +: merged, Nil, rTail)
            case (l :: lTail, Nil) => (l +: merged, lTail, Nil)
            case (l :: lTail, r :: rTail) if l < r => (l +: merged, lTail, r :: rTail)
            case (l :: lTail, r :: rTail) => (r +: merged, l :: lTail, rTail)
          }
        })._1.reverse

    if (input.length == 1) input
    else {
      val (left,right) = input.splitAt(input.length/2)
      val sortLeft = mergeSort(left)
      val sortRight = mergeSort(right)
      merge(sortLeft, sortRight)
    }
  }

  val myList = Seq(9,7,8,3,2,1)
  println(mergeSort(myList))

}
