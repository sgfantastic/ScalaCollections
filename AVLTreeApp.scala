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

    override def insert[B >: Nothing](newValue: B)(implicit ev: B => Ordered[B]): Tree[B] = ???

    override def inorder: Seq[Nothing] = Seq.empty[Nothing]
    override def preorder: Seq[Nothing] = Seq.empty[Nothing]
    override def postorder: Seq[Nothing] = Seq.empty[Nothing]

    override def levelorder: Seq[Nothing] = Seq.empty[Nothing]
  }



}
