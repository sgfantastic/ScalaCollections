package com.kanshu.functionalprogramming.sorting

object InsertionSortApp extends App{
  /**
   * Insertion Sort
   * i/p - 6,8,9,3,5
   * o/p - 3,5,6,8,9
   */

  def insertionSort[V](seq: Seq[V])(implicit enp: V => Ordered[V]): Seq[V] ={
    def insert(a: V, xs: Seq[V], acc: Seq[V]= Nil): Seq[V] ={
      xs match {
        case h :: t if a > h => insert(a, t , acc :+ h)
        case _ => (acc :+ a) ++ xs
      }
    }

    def sort(as: Seq[V], acc: Seq[V] = Nil): Seq[V] ={
      as match {
        case Nil => acc
        case h :: t => sort(t,insert(h, acc))
      }
    }
    sort(seq)
  }

  val mySeq = Seq(6,8,9,3,5)
  println(insertionSort(mySeq))

}
