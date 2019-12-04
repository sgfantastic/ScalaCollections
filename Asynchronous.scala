package part1recap
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global
object Asynchronous extends App{
  type Async[A] = (A => Unit) => Unit

  def timesTwo(n: Int): Async[Int] =
    onFinish => {
      val result = global.execute(() => onFinish(n*2))
    }
  timesTwo(20) {result => println(s"Result-0: $result")}

  sealed trait State
  case object Start extends State
  final case class WaitForA(b: Int) extends State
  final case class WaitForB(a: Int) extends State

  def timesFourInParallel(n: Int): Async[Int] = {
    onFinish => {
      var state: State = Start
      timesTwo(n) { a =>
        state match {
          case Start =>
            state = WaitForB(a)
          case WaitForA(b) =>
            onFinish(a + b)
          case WaitForB(_) =>
            throw new IllegalStateException(state.toString)
        }
      }
      timesTwo(n) { b =>
        state match {
          case Start =>
            state = WaitForA(b)
          case WaitForB(a) =>
            onFinish(a + b)
          case WaitForA(_) =>
            throw new IllegalStateException(state.toString)
        }
      }
      //  use synchronized blocks
      val lock = new AnyRef
      timesTwo(n) { a =>
        lock.synchronized{
          state match {
            case Start =>
              state = WaitForB(a)
            case WaitForA(b) =>
              onFinish(a+b)
            case WaitForB(_) =>
              throw new IllegalStateException(state.toString)
          }
        }
      }
    }
  }
  timesFourInParallel(20) { result => println(s"Result-1: $result") }

  //  AtomicReference
  import java.util.concurrent.atomic.AtomicReference

  def timesFourParallel(n: Int): Async[Int] = {
    onFinish => {
      val state = new AtomicReference[State](Start)
      def onValueA(a:Int): Unit =
        state.get match {
          case Start =>
            if(!state.compareAndSet(Start, WaitForB(a)))
              onValueA(a) // retry
          case WaitForA(b) =>
            onFinish(a + b)
          case WaitForB(_) =>
            throw  new IllegalStateException(state.toString)
        }
      timesTwo(n)(onValueA)

      def onValueB(b: Int): Unit =
        state.get match {
          case Start =>
            if(!state.compareAndSet(Start, WaitForA(b)))
              onValueB(b)
          case WaitForB(a) =>
            onFinish(a + b)
          case WaitForA(_) =>
            throw new IllegalStateException(state.toString)
        }
      timesTwo(n)(onValueB)
    }
  }

  timesFourParallel(40) { result => println(s"Result-2: $result") }

  // Recursivity (Wrath of StackOverflow)

  import java.util.concurrent.atomic.AtomicReference

  def mapBoth[A,B,R](fa: Async[A], fb: Async[B])(f:(A,B) => R): Async[R] = {
    //Define the state of machine
    sealed trait State[+A, +B]
    // Initial state
    case object Start extends State[Nothing, Nothing]
    // We got a B, waiting for an A
    final case class WaitForA[+B](b: B) extends State[Nothing, B]
    // We got a A, waiting for a B
    final case class WaitForB[+A](a: A) extends State[A, Nothing]
    onFinish => {
        val state = new AtomicReference[State[A,B]](Start)
        def onValueA(a: A): Unit =
          state.get match {
            case Start =>
              if (!state.compareAndSet(Start, WaitForB(a)))
                onValueA(a)  // retry
            case WaitForA(b) =>
              onFinish(f(a,b))
            case WaitForB(_) =>
              throw new IllegalStateException(state.toString)
          }
        def onValueB(b: B): Unit =
          state.get match {
            case Start =>
              if (!state.compareAndSet(Start, WaitForA(b)))
                onValueB(b) // retry
            case WaitForB(a) =>
              onFinish(f(a,b))
            case WaitForA(_) =>
              throw new IllegalStateException(state.toString)
          }
      fa(onValueA)
      fb(onValueB)
    }
  }
// define an operation similar to Scala's Future.sequence
  def sequence[A](list: List[Async[A]]): Async[List[A]] = {
    def loop(list: List[Async[A]], acc: Async[List[A]]): Async[List[A]] =
      list match {
        case Nil =>
          onFinish => acc(r => onFinish(r.reverse))
        case x :: xs =>
          val update  = mapBoth(x,acc)(_ :: _)
          loop(xs,update)
      }
    val empty: Async[List[A]] = _(Nil)
    loop(list,empty)
  }
  // Invocation
  sequence(List(timesTwo(10), timesTwo(20), timesTwo(30))) { r =>
    println(s"Result List:  $r")
  }

  // results in StackOverflowError
  val list = 0.until(1000).map(timesTwo).toList
  sequence(list)(r => println(s"Sum: ${r.sum}"))

  // Futures and Promises
  // The scala.concurrent.Future describes strictly evaluated asynchronous computations,
  // being similar to our Async type used above
  // Future and Promise are constructs used for synchronizing program execution
  // in some concurrent programming languages. They describe an object that acts
  // as a proxy for a result that is initially unknown, usually because the computation
  // of its value is yet incomplete.
  // The Future type describes asynchrony and not parallelism.
  // Yes, you can do things in parallel with it, but it's not meant only for parallelism
  // (async != parallelism) and for people looking into ways to use their CPU capacity to
  // its fullest, working with Future can prove to be expensive and unwise,
  // because in certain cases it has performance issues
  /*
  The properties of Future:
  * Eagerly evaluated - (strict and not lazy), meaning that when the caller of a function
  receives a Future reference, whatever asynchronous process that should complete it has
  probably started already
  * Memoized (cached) - , since being eagerly evaluated means that it behaves like a normal
  value instead of a function and the final result needs to be available to all listeners.
  The purpose of the value property is to return that memoized result or None if it isn't
  complete yet. Goes without saying that calling its def value yields a non-deterministic result.
  * Streams a single result and it shows because of the memoization applied.
  So when listeners are registered for completion, they'll only get called once at most.
  * The ExecutionContext manages asynchronous execution and although you can view it as
  a thread-pool, it's not necessarily a thread-pool
  (because async != multithreading or parallelism).
  * The onComplete is basically our Async type defined above, however it takes an
  ExecutionContext because all completion callbacks need to be called asynchronously
  * All combinators and utilities are built on top of onComplete, therefore all combinators
  and utilities must also take an ExecutionContext parameter.
   */

  import scala.concurrent.{Future, Promise}
  def timesTwoo(n: Int)(implicit ec: ExecutionContext): Future[Int] =
    Future(n * 2)
  // Usage
  import scala.concurrent.ExecutionContext.Implicits.global
  timesTwoo(20).onComplete{result => println(s"Result Future1:- $result")}

  def timesFouro(n: Int)(implicit ec: ExecutionContext): Future[Int] =
    timesTwoo(n).flatMap{ a =>
      timesTwoo(n).map { b =>
        a + b
      }
    }

  // Usage
  timesFouro(20).onComplete{result => println(s"Result Future2:- $result")}

  // parallelism
  def timesFourInParallell(n: Int) (implicit ec: ExecutionContext): Future[Int] = {
    val fa = timesTwoo(n)
    val fb = timesTwoo(n)

    for(a <- fa; b <- fb) yield a + b
  }
  timesFourInParallell(40).onComplete{result => println(s"Result4:- $result")}
}

