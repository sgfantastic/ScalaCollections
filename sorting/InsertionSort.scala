package com.kanshu.sparkscala.sorting

object InsertionSort extends App {

  class InsertionSort[+A] {
    def insertionSort[A](list: List[A])(implicit ev: A => Ordered[A]): List[A] = {
      def sort(as: List[A], bs: List[A]): List[A] = as match {
        case h :: t => sort(t, insert(h,bs))
        case Nil => bs
      }
      def insert(a: A, as: List[A]): List[A] = as match {
        case h :: t if (a > h) => h  :: insert(a,t)
        case _ => a :: as
      }
    sort(list,Nil)
    }
  }

  val myList: List[Int] = List(3,4,9,8,7,5,15,11,10)

  val iSort = new InsertionSort[Int]
  println(iSort.insertionSort(myList))

}
