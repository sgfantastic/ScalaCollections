package com.kanshu.scalafp

abstract class MyList[+A] {

  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyList[B]
  def printElement: String
  // polymorphic call
  override def toString: String = "[" + printElement + "]"
  def map[B](transformer: A => B): MyList[B]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]

  // concatenation
  def ++[B >: A](list: MyList[B]): MyList[B]

  def foreach(f: A => Unit): Unit
  def sort(compare: (A,A) => Int): MyList[A]
  def zipWith[B,C](list: MyList[B], zip:(A,B)=> C): MyList[C]
  def fold[B](start:B)(operator: (B,A) => B): B
}

object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing](element: B): MyList[B] = new Cons(element, Empty)
  def printElement: String = ""

  def map[B](transformer: Nothing => B): MyList[B] = Empty
  def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] = Empty
  def filter(predicate: Nothing => Boolean): MyList[Nothing] = Empty

  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
  def foreach(f : Nothing => Unit): Unit = ()
  def sort(compare: (Nothing, Nothing)=> Int): MyList[Nothing] = Empty
  def zipWith[B,C](list: MyList[B], zip: (Nothing,B) => C): MyList[C] =
    if (!list.isEmpty) throw new RuntimeException("List don not have the same length")
    else Empty
  def fold[B](start: B)(operator: (B,Nothing)=> B): B = start
}

class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): MyList[B] = new Cons(element,this)
  def printElement: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElement

  def map[B](transformer: A => B): MyList[B] =
    new Cons(transformer(h), t.map(transformer))

  def filter(predicate: A => Boolean): MyList[A] =
    if (predicate(h)) new Cons(h,t.filter(predicate))
    else t.filter(predicate)

  def ++[B >: A](list: MyList[B]): MyList[B] = new Cons(h, t ++ list)

  def flatMap[B](transformer: A => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)

  def foreach(f: A => Unit): Unit = {
    f(h)
    t.foreach(f)
  }

  def sort(compare: (A,A)=> Int): MyList[A] = {
    def insert(x: A, sortedList: MyList[A]): MyList[A] = {
      if (sortedList.isEmpty) new Cons(x , Empty)
      else if (compare(x,sortedList.head) <= 0) new Cons(x , sortedList)
      else new Cons(sortedList.head, insert(x,sortedList.tail))
    }
    val sortTail = t.sort(compare)
    insert(h, sortTail)
  }

  def zipWith[B,C](list: MyList[B], zip:(A,B)=> C): MyList[C] =
    if (list.isEmpty) throw new RuntimeException("List do not have same length")
    else new Cons(zip(h,list.head), t.zipWith(list.tail, zip))

  def fold[B](start: B)(operator: (B,A) => B): B =
    t.fold(operator(start,h))(operator)
}

//trait MyPredicate[-T] {
//  def test(elem: T): Boolean
//}
//
//trait MyTransformer[-A,B] {
//  def transform(elem: A): B
//}

object ListTest extends App {

  val list = new Cons(1, new Cons(2, Empty))
  println(list.tail.head)
  println(list.add(4).head)
  println(list.isEmpty)
  println(list.toString)

  val listOfIntegers: MyList[Int] = new Cons(1,new Cons(2, new Cons(3, Empty)))
  val anotherListOfIntegers: MyList[Int] = new Cons(4, new Cons(5, Empty))
  val listOfStrings: MyList[String] = new Cons("Hello", new Cons("scala", Empty))
  val listOfAny: MyList[Any] = new Cons(1, new Cons("hello", Empty))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)
  println(listOfAny.toString)

  println(listOfIntegers.map(new Function1[Int, Int] {
    override def apply(elem: Int): Int =  elem * 2
  }).toString)

  println(listOfIntegers.filter(new Function1[Int, Boolean] {
    override def apply(elem: Int): Boolean = elem % 2 == 0
  }).toString)

  println((listOfIntegers ++ anotherListOfIntegers).toString)
  println((listOfIntegers.flatMap(new Function1[Int, MyList[Int]] {
    override def apply(elem: Int): MyList[Int] = new Cons(elem, new Cons(elem + 1, Empty))
  })))

  listOfIntegers.foreach(println)
  println(listOfIntegers.sort((x,y)=> x - y))

  println(anotherListOfIntegers.zipWith[String,String](listOfStrings, _ + "-" + _))
  println(listOfIntegers.fold(0)(_ + _))

}
