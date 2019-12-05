package part1recap
object BinaryTreeApp extends App{
  case class Node[A](v: A, l: Tree[A], r: Tree[A]) extends Tree[A]
  case class Leaf[A](v: A) extends Tree[A]
  case object Empty extends Tree[Nothing]

  trait Tree[+A] {
    def value: Option[A] = this match {
      case n: Node[A] => Some(n.v)
      case l: Leaf[A] => Some(l.v)
      case Empty => None
    }
    def left: Option[Tree[A]] = this match {
      case n: Node[A] => Some(n.l)
      case l: Leaf[A] => None
      case Empty => None
    }
    def right: Option[Tree[A]] = this match {
      case n: Node[A] => Some(n.r)
      case l: Leaf[A] => None
      case Empty => None
    }
    private case class Eval[A](v: A) extends Tree[A]
    private def foldLoop[A,B](a: List[Tree[A]], z: B)(f:(B,A)=>B)(o:(Node[A], List[Tree[A]])=> List[Tree[A]]): B =
      a match {
        case (n: Node[A]) :: tl => foldLoop(o(n,tl),z)(f)(o) // never directly evaluate nodes, function o will create new accumulator
        case (l: Leaf[A]) :: tl => foldLoop(tl,f(z,l.v))(f)(o) // always evaluate Leaf
        case (e: Eval[A]) :: tl => foldLoop(tl, f(z,e.v))(f)(o) //  always evaluate Eval
        case Empty :: tl        => foldLoop(tl,z)(f)(o)  // ignore Empty
        case _                  => z // will be nil (Empty List)
      }
    def foldPreorder[B](z:B)(f:(B,A) => B): B = {
      foldLoop(List(this), z)(f){(n,tl) => Eval(n.v) :: n.l :: n.r :: tl}
    }
    def foldInorder[B](z:B)(f:(B,A) => B): B = {
      foldLoop(List(this),z)(f){(n,tl) => n.l :: Eval(n.v) :: n.r :: tl}
    }
    def foldPostorder[B](z: B)(f:(B,A) => B): B = {
      foldLoop(List(this),z)(f){(n,tl) => n.l :: n.r :: Eval(n.v) :: tl}
    }
    def foldLevelorder[B](z: B)(f:(B,A) => B): B ={
      foldLoop(List(this), z)(f){(n,tl) => (Eval(n.v) :: tl) ::: List(n.l,n.r)}
    }

    def fold[B](z: B)(f:(B,A) => B): B = foldInorder(z)(f)
    def size: Int = fold(0){(sum,v) => sum + 1}
    // The height of the tree is the length of the path from the root to the deepest node is the tree.
    // A (rooted) tree with only one node(the root) has a height of zero
    def height: Int ={
      def loop(t: Tree[A]): Int = t match {
        case l: Leaf[A] => 1
        case n: Node[A] => Seq(loop(n.left.get), loop(n.right.get)).max + 1
        case _          => 0
      }
      loop(this) -1
    }

    def leafCount:Int ={
      def loop(t: List[Tree[A]], z:Int): Int = t match {
        case (l: Leaf[A]) :: tl => loop(tl, z+1)
        case (n: Node[A]) :: tl => loop(n.left.get :: n.right.get :: tl, z)
        case _ :: tl            => loop(tl,z)
        case _                  => z
      }
      loop(List(this), 0)
    }

    def toSeq: Seq[A] = fold(List[A]()){(l,v) => v :: l}

    def toSeqPreorder: Seq[A] = foldPreorder(List[A]()) {(l,v) => v :: l} reverse
    def toSeqInorder: Seq[A] = foldInorder(List[A]()){(l,v) => v :: l} reverse
    def toSeqPostorder: Seq[A] = foldPostorder(List[A]()){(l,v) => v :: l}
    def toSeqLevelorder: Seq[A] = foldLevelorder(List[A]()){(l,v) => v :: l}

    //Find the last element in a binary tree using pre/in/post/level order traversals.
    def lastPreorder = toSeqPreorder.last
    def lastInorder: A = toSeqInorder.last
    def lastPostorder: A = toSeqPostorder.last
    def lastLevelorder: A = toSeqLevelorder.last

    //Find the last but one element in a binary tree using pre/in/post/level order traversals.
    def penultimatePreorder = toSeqPreorder.dropRight(1).last
    def penultimateInorder: A = toSeqInorder.dropRight(1).last
    def penultimatePostorder: A = toSeqPostorder.dropRight(1).last
    def penultimateLevelorder: A = toSeqLevelorder.dropRight(1).last

    //Find the Nth element in a binary tree using pre/in/post/level order traversals.
    def nthPreorder(n: Int) = toSeqPreorder(n)
    def nthInorder(n: Int) = toSeqInorder(n)
    def nthPostorder(n: Int) = toSeqPostorder(n)
    def nthLevelorder(n: Int) = toSeqLevelorder(n)

    // Binary Search Pre-order
    def searchPreorder[B >: A](key: B): Option[A] = {
      def helperSearchPreorder(lst: Seq[A]): Option[A] = lst match {
        case Seq() => None
        case h :: tl =>
          if (h == key) Some(h)
          else helperSearchPreorder(tl)
      }
      helperSearchPreorder(this.toSeqPreorder)
    }

    def searchBTree[B >: A](seq: Seq[B], key: B) : Option[B] = seq match {
      case Seq() => None
      case h :: tl =>
        if (h == key) Some(h)
        else searchBTree(tl, key)
    }

     // Binary Search Pre-order
   def searchBinaryTree[B >: A](traversalType: String, key: B): Option[B] = traversalType match {
     case "Inorder" => searchBTree(this.toSeqInorder, key)
     case "Postorder" => searchBTree(this.toSeqPostorder, key)
     case "Preorder" => searchBTree(this.toSeqPreorder, key)
     case "Levelorder" => searchBTree(this.toSeqLevelorder, key)
     case _            => None
   }
  }

  val t: Tree[Char] = Node('F', Node('B', Leaf('A'),Node('D', Leaf('C'), Leaf('E'))), Node('G', Empty, Node('I', Leaf('H'), Empty)))
  println("tree: ", t)
  for {
    b <- t.left
    value <- b.value
  } println("B node: " + value)

  for {
    b <- t.left
    d <- b.right
    value <- d.value
  } println("D node: " + value)

  println(t.size)
  println(t.height)
  println(t.leafCount)

  println("as seqPreorder: ", t.toSeqPreorder)
  println("as seqInorder: ", t.toSeqInorder)
  println("as seqPostorder: ", t.toSeqPostorder)
  println("as seqLevelorder: ", t.toSeqLevelorder)

  println("last preorder: " + t.lastPreorder)
  println("last inorder: "+ t.lastInorder)
  println("last postorder: " + t.lastPostorder)
  println("last levelorder: "+ t.lastLevelorder)

  println("nth preorder 5 : " + t.nthPreorder(5))
  println("nt inorder 5 : " + t.nthInorder(5))
  println("nth postorder 5 : " + t.nthPostorder(5))
  println("nth levelorder 5 : " + t.nthLevelorder(5))

  println(t.toSeq.reverse)

  println(t.searchBinaryTree("Inorder",'A'))
  println(t.searchBinaryTree("Inorder",'Z'))
  
  println(t.searchBinaryTree("Preorder",'B'))
  println(t.searchBinaryTree("Preorder",'Z'))
  
  println(t.searchBinaryTree("Postorder",'C'))
  println(t.searchBinaryTree("Postorder",'Z'))
  
  println(t.searchBinaryTree("Levelorder",'D'))
  println(t.searchBinaryTree("Levelorder",'Z'))
}
