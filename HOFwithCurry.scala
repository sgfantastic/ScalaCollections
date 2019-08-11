package com.kanshu.scala

object HOFsCurries extends App {
  def ntimes(f: Int => Int, n: Int, x: Int): Int =
    if (n <= 0) x
    else ntimes(f, n-1, f(x))

  val plusOne: Int => Int = (x: Int) => x + 1

  println(ntimes(plusOne,10,1))

//  val superFunction: (Int, (String,(Int => Boolean))=> Int) => (Int => Int) = ???

  // curried function
  def nTimesBetter(f: Int => Int, n: Int): (Int => Int) =
    if (n <= 0) (x: Int) => x
    else (x: Int) => nTimesBetter(f,n-1)(f(x))

  val supFunc: ((Int => Int), Int) =>(Int => Int) =
    (f: Int => Int, n: Int) =>   if (n <= 0) (x: Int) => x
    else (x: Int) => nTimesBetter(f,n-1)(f(x))


  val plus10 = nTimesBetter(plusOne,10)
  val plus12 = supFunc(plusOne, 12)
  println(plus10(1))
  println("12 + ", plus12(1))


  val superAdder: Int => (Int => Int) = (x: Int)  => (y: Int) => x + y
  val adder3 = superAdder(3)
  println(adder3(10))
  println(superAdder(2)(3))

  def curriedFormatter(c: String)(x: Double): String = c.format(x)
  val standardFormatter: (Double => String) = curriedFormatter("%4.2f")
  println(standardFormatter(math.Pi))

  def toCurry(f: (Int, Int)=> Int): (Int => Int => Int) =
    x => y => f(x,y)

  def fromCurry(f: (Int => Int => Int)): (Int, Int) => Int =
    (x,y) => f(x)(y)

  def compose(f: Int=> Int, g: Int => Int): Int =>Int =
    x => f(g(x))

  def andThen(f: Int=> Int, g: Int => Int): Int =>Int =
    x => g(f(x))

  def superAdder2: (Int => Int => Int) = toCurry(_ + _)
  def add4 = superAdder2(4)
  println(add4(17))

  val simpleAdder = fromCurry(superAdder)
  println(simpleAdder(4,17))

  val add2 = (x: Int) => x + 2
  val times3 = (x: Int) => x * 3

  val composed = compose(add2, times3) // 4 + 2 * 3
  val ordered = andThen(add2,times3)

  println(composed(4))
  println(ordered(4))

}
