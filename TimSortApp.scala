package com.kanshu.functionalprogramming.sorting

object TimSortApp extends App{
  /**
   * tim sort -> merge sort + insertion sort
   * i/p - (5,4,1,2,3,0,8,7,9,11,12,6)
   * o/p - (0,1,2,3,4,5,6,7,8,9,11,12)
   */

  val minRun = 3
  // insertion sort
  def insertionSort[V](seq: Seq[V])(implicit env: V => Ordered[V]): Seq[V] ={
    def insert(a: V, bs: Seq[V], acc : Seq[V] = Nil): Seq[V] = {
      bs match {
        case h :: t if a > h => insert(a, t, acc :+ h)
        case _ => (acc :+ a) ++ bs
      }
    }
    def sort(as: Seq[V], acc: Seq[V]= Nil): Seq[V] ={
      as match {
        case Nil => acc
        case h :: t => sort(t, insert(h,acc))
      }
    }
    sort(seq)
  }

  val mySeq = Seq(5,4,1,2,3,0,8,7,9,11,12,6)

  // merge

  def timSort[V](input: Seq[V])(implicit env: V => Ordered[V]): Seq[V] ={
    def merge(left: Seq[V], right: Seq[V]): Seq[V] ={
      (0 until left.length + right.length).foldLeft(Seq[V](), left, right)((triple, _)=>{
        val (merged, leftRemaining, rightRemaining) = triple
        (leftRemaining, rightRemaining) match {
          case (Nil, r::rTail) => (r +: merged, Nil, rTail)
          case (l::lTail, Nil) => (l +: merged, lTail, Nil)
          case (l::lTail, r ::rTail) if l < r => (l +:merged, lTail, r::rTail)
          case (l::lTail, r ::rTail) => (r+:merged, l::lTail, rTail)
        }
      })._1.reverse
    }

    if (input.length <= minRun) insertionSort(input)
    else {
      val (left,right) = input.splitAt(input.length/2)
      val leftSorted = timSort(left)
      val rightSorted = timSort(right)
      merge(leftSorted, rightSorted)
    }
  }

  println(timSort(mySeq))

}
