package com.kanshu.sparkscala.sorting

object QuickSort extends App {
  class QuickSort[+A] {
    def quickSort[A](xs :List[A])(implicit ev: A => Ordered[A]): List[A] = {
      if (xs.length <= 1) xs
      else {
        val pivot = xs(xs.length/2)
        List.concat(
          quickSort(xs.filter(pivot > )),
          xs filter (pivot ==),
          quickSort(xs filter (pivot < ))
        )
      }
    }
  }

  val x: List[Int] = List(9,7,6,5,4,3,8,1,2)
  val qSort = new QuickSort[Int]
  println(qSort.quickSort(x))

  val ch: List[Char] = List('a', 'b','z','e','t','x','u','m')
  val chqSort = new QuickSort[Char]
  println(chqSort.quickSort(ch))
}
