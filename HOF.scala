 package com.kanshu.scala.HOF
 
 object HOFScala extends App {
 
 trait MyTest[+T] {
    def test[B](f: (T,T)  => B): B
  }

  class Ntest[+T](x: T, y: T) extends MyTest[T] {
    def test[B](f:(T,T) => B) = f(x,y)
  }
  val d = new Ntest[Int](10, 20)
  println("d = " + d.test(new Function2[Int,Int,Int]{
    override def apply(x : Int, y: Int): Int = x + y
  }))
  
  abstract class MynTest[+T] {
    def test[B](f: T  => B): B
  }

  class Nntest[+T](x: T) extends MynTest[T] {
    def test[B](f:T => B) = f(x)
  }
  val d = new Nntest[Int](10)
  println("d = ", d.test(new Function1[Int,Int]{
    override def apply(x : Int): Int = x + 1
  }))
  
  }
