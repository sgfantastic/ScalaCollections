object StackAndQueueApp extends App{
/*
A stack is a linear data structure that follows the last in first out (LIFO)
Elements are added and removed from the same end
 */
  abstract sealed class Stack[+A] {
    def head: A
    def tail: Stack[A]
    def isEmpty: Boolean
    def add[B >: A](element: B): Stack[B]
    def pop: Stack[A]
    def length: Int
  }

  case object Empty extends Stack[Nothing] {
    override def head: Nothing = throw new NoSuchElementException
    override def tail: Stack[Nothing] = throw new NoSuchElementException
    override def isEmpty: Boolean = true
    override def add[B >: Nothing](element: B): Stack[B] = Cons(element, Empty)
    override def pop: Stack[Nothing] = Empty
    def length: Int = 0
  }

  case class Cons[+A](h: A, t: Stack[A]) extends Stack[A] {
    override def head: A = h
    override def tail: Stack[A] = t
    override def isEmpty: Boolean = false
    override def add[B >: A](element: B): Stack[B] = Cons(element, this)
    override def pop: Stack[A] = tail
    override def length: Int = getLength(this)
    private def getLength[A](s : Stack[A] , acc: Int = 0): Int = {
      s match {
        case Empty => acc
        case Cons(_,xt) => getLength(xt, acc + 1)
      }
    }

  }
/*
  val lst = Cons(1, Empty)
  val lst1 = lst.add(2).add(3).add(4)
  println(lst1)
  println(lst1.length)
  println(lst1.head)
  println(lst1.pop)
  println(lst1.pop.length)
  println(lst1.pop.pop.pop.pop.pop)
*/

  abstract sealed class MyQueue[+A] {
    def peek: A
    def tail: MyQueue[A]
    def isEmpty: Boolean
    def enqueue[B >: A](element: B): MyQueue[B]
    def dequeue: MyQueue[A]
    def length: Int
    def ++[B >: A](queue: MyQueue[B]): MyQueue[B]
  }

  case object EmptyQueue extends MyQueue[Nothing] {
    override def peek: Nothing = throw new NoSuchElementException
    override def tail: MyQueue[Nothing] = throw new NoSuchElementException
    override def isEmpty: Boolean = true
    override def enqueue[B >: Nothing](element: B): MyQueue[B] = ConsQueue(element, EmptyQueue)
    override def dequeue: MyQueue[Nothing] = EmptyQueue
    override def length: Int = 0
    def ++[B >: Nothing](queue: MyQueue[B]): MyQueue[B] = queue
  }

  case class ConsQueue[+A](h: A, t: MyQueue[A]) extends MyQueue[A] {
    override def peek: A = h
    override def tail: MyQueue[A] = t
    override def isEmpty: Boolean = false
    override def enqueue[B >: A](element: B): MyQueue[B] = this ++ ConsQueue(element, EmptyQueue)
    override def dequeue: MyQueue[A] = t
    override def length: Int = ???
    def ++[B >: A](queue: MyQueue[B]): MyQueue[B] = ConsQueue(h, t ++ queue)
  }

  val que = ConsQueue(1, EmptyQueue)
  val que1 = que.enqueue(2).enqueue(3).enqueue(4)

  val lst = Cons(1, Empty)
  val lst1 = lst.add(2).add(3).add(4)


  println(que1)
  println(que1.peek)
  println(que1.dequeue)

  println(lst1)
  println(lst1.head)
