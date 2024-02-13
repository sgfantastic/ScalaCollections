import scala.collection.mutable.PriorityQueue

object HuffmanTreeApp extends App{

  /*
  val charFreq = List(('a', 5), ('b', 9), ('c', 12), ('d', 13), ('e', 16), ('f', 45))
  val originalString = "abacabadabacaba"
  HuffmanTree:- 
                      100
               0  f45      55 1
                      0 25      30  1
                 0 c12  d13   14    e16   1
                           a5  b9
 */

  case class Node(char: Char, freq: Int, left: Option[Node] = None, right: Option[Node] = None)

  def buildHuffmanTree(charFreq: List[(Char, Int)]): Option[Node] = {
    val priorityQueue = PriorityQueue[Node]()(Ordering.by(-_.freq))

    charFreq.foreach { case (char, freq) =>
      priorityQueue.enqueue(Node(char, freq))
    }

    println(priorityQueue)

    while (priorityQueue.size > 1) {
      val left = priorityQueue.dequeue()
      val right = priorityQueue.dequeue()
      val internalNode = Node('\0', left.freq + right.freq, Some(left), Some(right))
      priorityQueue.enqueue(internalNode)
    }

    priorityQueue.headOption
  }

  def assignCodes(root: Option[Node], code: String = ""): Map[Char, String] = {
    root match {
      case Some(node) if node.char != '\0' =>
        Map(node.char -> code)
      case Some(node) =>
        assignCodes(node.left, code + "0") ++ assignCodes(node.right, code + "1")
      case None => Map.empty
    }
  }

  def huffmanEncode(originalString: String, charFreq: List[(Char, Int)]): String = {
    val root = buildHuffmanTree(charFreq)
    println(root)
    val codes = assignCodes(root)
    println(codes)
    originalString.flatMap(c => codes(c))
  }

  val charFreq = List(('a', 5), ('b', 9), ('c', 12), ('d', 13), ('e', 16), ('f', 45))
  val originalString = "abacabadabacaba"

  val encodedString = huffmanEncode(originalString, charFreq)
  println(s"Original String: $originalString")
  println(s"Encoded String: $encodedString")

}
