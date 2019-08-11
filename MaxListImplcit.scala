package com.kanshu.scala

object ImplicitApp extends App {

  val lst = List(8,4,5,6,11,0,10)
  val d = new A
  println(d.maxList(lst))
  val e = new B
  println(e.maxList(lst))

  val c = new C
  println(c.maxList(lst))
  var x = 0
  for (i <- lst) {
    if (x > i) x else x = i
  }
  println("x= "+ x)
}

// Using Context bound
class A {
  def maxList[T : Ordering] (elements: List[T]): T = elements match {
    case List() => throw new IllegalArgumentException("Empty List")
    case List(x) => x
    case x :: rest =>
      val maxRest = maxList(rest)
      if (implicitly[Ordering[T]].gt(x,maxRest)) x
      else maxRest
  }
}

// implicit with ordering
class B {
  def maxList[T](elements: List[T])(implicit ordering: Ordering[T]) : T = elements match {
    case List() => throw new IllegalArgumentException("empty List")
    case List(x) => x
    case x :: rest =>
      val maxRest = maxList(rest)
      if(implicitly [Ordering[T]].gt(x,maxRest)) x
      else maxRest
  }
}

// Implicit with ordered
class C {
  def maxList[T](elements: List[T])(implicit ordering: T => Ordered[T]): T = elements match {
    case List() => throw new IllegalArgumentException("empty list")
    case List(x) => x
    case x :: rest =>
      val maxRest = maxList(rest)
      if (x > maxRest) x
      else maxRest
  }
}
