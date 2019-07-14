package com.kanshu.sparkscala.sorting

object BubbleSort extends App {

  class BubbleSort[+A] {
    // View Bound :-
    // A view bound was a mechanism introduced in Scala to enable the use of some type A as if it were some type B
    // A should have an implicit conversion to B available, so that one can call B methods on an object of type A.
    // The most common usage of view bounds in the standard library (before Scala 2.8.0, anyway),
    // is with Ordered,
    // def bubblesort[A <% Ordered[A]](list: List[A]): List[A] = {
    def bubblesort[A](list: List[A])(implicit ev: A => Ordered[A]): List[A] = {
      def sort(as: List[A], bs: List[A]): List[A] = as match {
        case as if as.isEmpty => bs
        case _ => bubble(as, Nil,bs)
      }

      def bubble(as: List[A], zs: List[A], bs: List[A]): List[A] = as match {
        case h1 :: h2 :: tail =>
          if (h1 > h2) bubble(h1 ::tail, h2 ::zs, bs)
          else bubble(h2 :: tail, h1 :: zs, bs)
        case h1 :: Nil => sort(zs, h1:: bs)
      }

      sort(list, Nil)
    }
  }

  val myList: List[Int] = List(4,3,5,9,2,1,11,12)
  val bsort = new BubbleSort[Int]
  println(bsort.bubblesort(myList))

//  def f[A <% Int](a: A) = ???
//  def f[A](a: A)(implicit ev: A => Int) = ???
}
