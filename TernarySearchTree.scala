object TSTApp extends App{
  val ternary = Ternary[Boolean]
    .insert("Apple", true)
    .insert("Mango", true)
    .insert("Potato", true)

  println(ternary.keys)
  println(ternary.search("Apple"))
}

sealed trait Ternary[+A] {
  def insert[B >: A](key: String, value: B): Ternary[B] = Ternary.insert(this, key,value,0)
  def search(key: String): Option[A] = Ternary.search(this,key,0)
  def keys: List[String] = Ternary.keys(this)
  def keyWithPrefix(prefix: String): List[String] = Ternary.keys(this,prefix)
}

case class Node[V](value: Option[V], char: Char, left: Ternary[V], mid: Ternary[V], right: Ternary[V]) extends Ternary[V]
case object Leaf extends Ternary[Nothing]

object Ternary {
  def apply[V]: Ternary[V] = Leaf
  private def keys[A](root: Ternary[A]): List[String] = collect(root, "")
  private def keys[A](root: Ternary[A], prefix: String): List[String] =
    get(root, prefix, 0) match {
      case None => Nil
      case Some(node) =>
        collect(node,prefix.dropRight(1))
    }
  private def collect[A](node: Ternary[A], prefix: String): List[String] = {
    node match {
      case Leaf => Nil
      case node: Node[A] if node.value.isDefined =>
        (prefix + node.char) +: (collect(node.left, prefix) ++ collect(node.mid, prefix + node.char) ++ collect(node.right, prefix))
      case node: Node[A] =>
        collect(node.left, prefix) ++ collect(node.mid, prefix + node.char) ++ collect(node.right, prefix)
    }
  }
  private def get[A](root:Ternary[A], prefix: String, step: Int): Option[Ternary[A]] = root match {
    case Leaf => None
    case node:Node[A] if node.char > prefix.charAt(step) => get(node.left,prefix,step)
    case node: Node[A] if node.char < prefix.charAt(step) => get(node.right, prefix,step)
    case node: Node[A] if step < prefix.length -1 => get(node.mid, prefix, step + 1)
    case node: Node[A] => Some(node)
  }
  private def search[A](root: Ternary[A], key: String, step: Int): Option[A] = root match{
    case Leaf => None
    case node: Node[A] if node.char > key.charAt(step) => search(node.left, key,step)
    case node: Node[A] if node.char < key.charAt(step) => search(node.right,key,step)
    case node: Node[A] if step < key.length -1 => search(node.mid,key,step + 1)
    case node: Node[A] => node.value
  }
  private def insert[A](root: Ternary[A], key: String, value: A, step: Int): Ternary[A] = root match {
    case Leaf =>
      val node = Node(None, key.charAt(step), Leaf, Leaf,Leaf)
      insert(node, key, value,step)
    case node: Node[A] if node.char > key.charAt(step) =>
      val left = insert(node.left, key,value,step)
      node.copy(left = left)
    case node: Node[A] if node.char < key.charAt(step) =>
      val right = insert(node.right, key,value,step)
      node.copy(right = right)
    case node: Node[A] if step < key.length -1  =>
      val mid = insert(node.mid, key, value, step + 1 )
      node.copy(mid = mid)
    case node: Node[A] =>
      node.copy(value = Some(value))
  }
}
