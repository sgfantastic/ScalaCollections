package SortAlgo

import scala.annotation.tailrec

object SelectionSort extends App{
  def minimum[T](xs: List[T])(implicit ev:T => Ordered[T]): List[T] =
    (List(xs.head) /: xs.tail) {
      (ys,x) =>
        if(x < ys.head) (x :: ys)
        else (ys.head :: x :: ys.tail)
    }

  def selectionSort[T](xs: List[T])(implicit ev:T => Ordered[T]): List[T] = xs match {
    case List() => List()
    case _ =>
      val ys = minimum(xs)
      if (ys.tail.isEmpty) ys
      else ys.head :: selectionSort(ys.tail)
  }
  // tail recursive
  def tailSelectionSort[T](xs: List[Int]) = {
    @tailrec
    def helper[T](xs: List[T], acc: List[T])(implicit ev: T => Ordered[T]): List[T] = xs match {
      case List() => acc
      case _ =>
        val ys = minimum(xs)
        helper(ys.tail, ys.head :: acc)
    }
    helper(xs,Nil)
  }


  val list: List[Int] = List(8,7,6,9,5)
  println(minimum(list))
  println(selectionSort(list))
  println(tailSelectionSort(list))
  println(list.foldLeft(List[Int]()){(x,y) => x :+ y*y})

  val nlist: List[Int] = List(1,1,2,3,4,4,5,6,7,6,7,7,8,9,10)

  println(nlist.foldLeft(List[Int]()){
    case (List(),e) => List(e)
    case(ls, e) if (ls.last == e) => ls
    case(ls,e) => ls ::: List(e)
  })
  println(nlist.last)


}
