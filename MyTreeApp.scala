package com.kanshu.scala

object MyTreeApp extends App{
  val bt = new ConsTree(EmptyTree,100,EmptyTree)
  val btree = bt + 201 + 50 + 10 + 6 + 7 + 3 + 4
  println(btree)
  println(btree.map(new Function1[Int, Int] {
    override def apply(elem: Int): Int = elem * 2
  }))
  println(btree.map(e => e * 3))
  println(btree.filter(new Function1[Int, Boolean]{
    override def apply(elem: Int): Boolean = elem % 2 == 0
  }))

  println(btree.filter(e => e % 2 == 1))

}

abstract class MyTree[+A]  {
  def head : A
  def left: MyTree[A]
  def right: MyTree[A]
  def isEmpty: Boolean
  def + [B >: A](elem: B)(implicit ev: B => Ordered[B]): MyTree[B]
  def map[B](transformer: A => B): MyTree[B]
  def ++[B >: A] (tree : MyTree[B])(implicit ev: B => Ordered[B]): MyTree[B]
  def flatMap[B](transformer: A => MyTree[B])(implicit ev: B => Ordered[B]): MyTree[B]
  def filter(predicate: A => Boolean): MyTree[A]

  def printElement: String
  override def toString = "[" + printElement + "]"
}

case object EmptyTree extends MyTree[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def left: MyTree[Nothing] = throw new NoSuchElementException
  def right: MyTree[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def + [B >: Nothing](elem: B)(implicit ev: B => Ordered[B]): MyTree[B] = new ConsTree(EmptyTree,elem,EmptyTree)
  def map[B](transformer: Nothing => B): MyTree[B] = EmptyTree
  def ++[B >: Nothing] (tree : MyTree[B])(implicit ev: B => Ordered[B]): MyTree[B] = tree
  def flatMap[B](transformer: Nothing => MyTree[B])(implicit ev: B => Ordered[B]): MyTree[B] = EmptyTree
  def filter(predicate: Nothing => Boolean): MyTree[Nothing] = EmptyTree
  def printElement: String = ""
}

case class ConsTree[+A](l: MyTree[A], h: A, r: MyTree[A]) extends MyTree[A]{
  def head : A  = h
  def left: MyTree[A] = l
  def right: MyTree[A] = r
  def isEmpty : Boolean = false
  def + [B >: A](elem: B)(implicit ev: B => Ordered[B]): MyTree[B] = {
    if (this.isEmpty) new ConsTree(left,elem,right)
    else if (elem <= head) new ConsTree(left + elem, head , right)
    else new ConsTree(left, head, right + elem )
  }
  def printElement: String = {
    if (l.isEmpty && r.isEmpty) "" + h
    else h + " " + l.printElement + " " + r.printElement
  }
  def map[B](transformer: A => B): MyTree[B] = new ConsTree[B](l.map(transformer),transformer(h), r.map(transformer))

  def ++[B >: A] (tree : MyTree[B])(implicit ev: B => Ordered[B]): MyTree[B] = {
    if (tree.head > head) new ConsTree(l,h,r ++ tree)
    else new ConsTree(l ++ tree , h , r)
  }
  def flatMap[B](transformer: A => MyTree[B])(implicit ev: B => Ordered[B]): MyTree[B] =
    transformer(h) ++ l.flatMap(transformer) ++ r.flatMap(transformer)
  def filter(predicate: A => Boolean): MyTree[A] =
    if (predicate(h)) new ConsTree(l.filter(predicate), h, r.filter(predicate))
    else { l.filter(predicate)
      r.filter(predicate)}

}


