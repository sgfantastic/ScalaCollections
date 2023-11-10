package com.kanshu.datastructures

object AVLTreeApp extends App {

  /**
  Input - Seq(10,5,20,15,25,12,4,50)
  AVL Tree -
               10
             /    \
           5       20
         / \      /   \
        4   E    15    25
      /  \      /  \   / \
     E    E   12   E  E   50
  E - Empty Node

   *  What is an AVL Tree ?
   *  Balance Factor: -1 , 0, 1 = left.height - right.height
   *  Self-Balancing:
   *  Height-Balanced:
   *  Operations:
   *  Rotations:
   *  Complexity:
   *  Use Cases:
   *  Disadvantages:
   */
  sealed trait Tree[+A]{
    def value: A
    def left: Tree[A]
    def right: Tree[A]

    def height: Int
    def balanceFactor: Int

    def rotateRight: Tree[A]
    def rotateLeft: Tree[A]
    def rotateLeftRight: Tree[A]
    def rotateRightLeft: Tree[A]
    def balance: Tree[A]

    def insert[B >:A](newValue: B)(implicit ev: B => Ordered[B]): Tree[B]

    // DFS
    def inorder: Seq[A]
    def preorder: Seq[A]
    def postorder: Seq[A]

    // BFS
    def levelorder: Seq[A]
  }

  case object Empty extends Tree[Nothing] {
    override def value: Nothing = throw new NotImplementedError("the value is empty")
    override def left: Tree[Nothing] = Empty
    override def right: Tree[Nothing] = Empty
    override def height: Int = 0
    override def balanceFactor: Int = 0

    override def rotateRight: Tree[Nothing] = Empty
    override def rotateLeft: Tree[Nothing] = Empty
    override def rotateLeftRight: Tree[Nothing] = Empty
    override def rotateRightLeft: Tree[Nothing] = Empty

    override def balance: Tree[Nothing] = Empty

    override def insert[B >: Nothing](newValue: B)(implicit ev: B => Ordered[B]): Tree[B] =
      Node(newValue, Empty, Empty)

    override def inorder: Seq[Nothing] = Seq.empty[Nothing]
    override def preorder: Seq[Nothing] = Seq.empty[Nothing]
    override def postorder: Seq[Nothing] = Seq.empty[Nothing]

    override def levelorder: Seq[Nothing] = Seq.empty[Nothing]
  }

  case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A] {
    override def height: Int = 1 + Math.max(left.height, right.height)
    override def balanceFactor: Int = left.height - right.height

    override def rotateRight: Tree[A] = this match {
      case Node(parent, l , Node(rv, rl,rr)) => Node(rv, Node(parent, l, rl), rr)
    }

    override def rotateLeft: Tree[A] = this match {
      case Node(parent, Node(lv, ll , lr), r) => Node(lv, ll, Node(parent,lr, r))
    }

    override def rotateLeftRight: Tree[A] =
      Node(left.value, left.rotateLeft, right).rotateRight

    override def rotateRightLeft: Tree[A] =
      Node(right.value, left, right.rotateRight).rotateLeft

    override def balance: Tree[A] =
      if (balanceFactor > 1) {
        if(left.balanceFactor <= 0) rotateLeft else rotateLeftRight
      } else if (balanceFactor < -1) {
        if(right.balanceFactor >= 0) rotateRight else rotateRightLeft
      } else this

    override def insert[B >: A](newValue: B)(implicit ev: B => Ordered[B]): Tree[B] =
      if(newValue < value) {
        val newLeft = left.insert(newValue).balance
        Node(value, newLeft, right)
      } else if (newValue > value) {
        val newRight = right.insert(newValue).balance
        Node(value, left, newRight)
      } else this

    override def inorder: Seq[A] = left.inorder ++ Seq(value) ++ right.inorder
    override def preorder: Seq[A] = Seq(value) ++ left.preorder ++ right.preorder
    override def postorder: Seq[A] = left.postorder ++ right.postorder ++ Seq(value)

    override def levelorder: Seq[A] = {
      def loop(que: Seq[Tree[A]], acc: Seq[A] = Seq[A]()): Seq[A] = {
        que match {
          case Nil => acc
          case head :: tail =>
            head match {
              case Empty => loop(tail,acc)
              case Node(vl, left, right) =>
                val newAcc = acc :+ vl
                val newQue = tail ++ Seq(left,right)
                loop(newQue, newAcc)
            }
        }
      }
      loop(Seq(this))
    }
  }

  val avlTree =
    Seq(10,5,20,15,25,12,4,50).foldLeft(Empty: Tree[Int])((avlT,v ) => avlT.insert(v))

  println(avlTree)
  println(avlTree.inorder)
  println(avlTree.preorder)
  println(avlTree.postorder)

  println(avlTree.levelorder)

}
