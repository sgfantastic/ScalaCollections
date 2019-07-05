package lectures.part3concurrency

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.duration._

object FuturePromise extends App {
  def calculateMeaningOfLife: Int ={
    Thread.sleep(1000)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife
  }  // (global) which is passed by the compiler

  println(aFuture.value)   // Option[Try[Int]]

  println("waiting for the future")
//  aFuture.onComplete(t => t match {
//    case Success(meaningOfLife) => println(s"the meaning of life is $meaningOfLife")
//    case Failure(e) => println(s"I have failed with $e")
//  })

  aFuture.onComplete {
    case Success(meaningOfLife) => println(s"the meaning of life is $meaningOfLife")
    case Failure(e) => println(s"I have failed with $e")
  }

  Thread.sleep(3000)

  // mini social network

  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile): Unit = {
      println(s"${this.name} poking ${anotherProfile.name}")
    }
  }

  object SocialNetwork {
    // database
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-dummy" -> "Dummy"
    )

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )

    val random = new Random()

    //API
    def fetchProfile(id: String): Future[Profile] = Future {
      // fetching from the DB
      Thread.sleep(random.nextInt(300))
      Profile(id,names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bfid = friends(profile.id)
      Profile(bfid, names(bfid))
    }
  }

  // client: mark to poke bill
  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")
//  mark.onComplete{
//    case Success(markProfile) => {
//      val bill = SocialNetwork.fetchBestFriend(markProfile)
//      bill.onComplete{
//        case Success(billProfile) => markProfile.poke(billProfile)
//        case Failure(e) => e.printStackTrace()
//      }
//    }
//    case Failure(ex) => ex.printStackTrace()
//  }



  // Functional Composition of Futures
  // map, flatMap, filter

  val nameOnTheWall = mark.map(profile => profile.name)

  val marksBestFriend = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile))

  val zuckBestFriendRestricted = marksBestFriend.filter(profile => profile.name.startsWith("z"))

  // for comprehension

  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriend(mark)
  } mark.poke(bill)

  Thread.sleep(1000)

  // fallback  recover recoverWith fallbackTo

  val aProfileNOmatterWhat = SocialNetwork.fetchProfile("unknow-id").recover{
//    case e: Throwable => Profile("fb.id.0-dummy", "Forever Alone")
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknow-id").recoverWith{
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  val fallBackResult = SocialNetwork.fetchProfile("unknow-id").fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy"))

  // online banking app
  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object Banking {
    val name = "Rock the JVM"

    def fetchUser(name: String): Future[User] = Future {
      // simulate fetching from the DB
      Thread.sleep(500)
      User(name)
    }

    def createTranasaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      // simulate some process
      Thread.sleep(1000)
      Transaction(user.name,merchantName, amount, "SUCCESS")
    }

    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      // fetch the user from the DB
      // create a transaction
      // wait for transaction to finish

      val transactionStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTranasaction(user,merchantName,cost)
      }yield transaction.status

      Await.result(transactionStatusFuture, 2.seconds)   // implicit conversion -> pimp my library
    }
  }

  println(Banking.purchase("Anshu","shoes", "adidas", 3000))

  // promises
  val promise = Promise[Int]()   // "controller" over future
  val future = promise.future

  // thread 1 - "consumer"

  future.onComplete{
    case Success(r) => println("[consumer] I've received " + r)
    case Failure(_) =>
  }

  // thread 2 - "producer"

  val producer = new Thread(new Runnable {
    override def run(): Unit = {
      println("[producer] crunching numbers.....")
      Thread.sleep(500)
      // "fulfilling" the promise
      promise.success(42)
      println("[producer] done")
    }
  })

  producer.start()
  Thread.sleep(1000)

  /*
    1) fulfil a future IMMEDIATELY with a value
    2) inSequence(f1,fb)
    3) first(fa,fb) => new future with the first value of the two futures
    4) last(fa, fb) => new future with the last value
    5) returnUntil(action: () => Future[T], condition: T => Boolean): Future[T]
   */
  // 1 - fulfill immediately
  def fulfillImmediately[T](value: T): Future[T] = Future(value)

  // 2 - insequence
  def inSequence[A,B](first: Future[A], second: Future[B]): Future[B] =
    first.flatMap(_ => second)

  // 3  - first out of two futures
  def first[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val promise = Promise[A]
    /*
        def tryComplete(promise: Promise[A], result: Try[A]): Unit = result match {
          case Success(r) => try {
            promise.success(r)
          } catch {
            case _ =>
          }
          case Failure(t) => try {
            promise.failure(t)
          } catch {
            case _ =>
          }
        }

    //    fa.onComplete(result => tryComplete(promise,result))
        fa.onComplete(tryComplete(promise,_))
        fb.onComplete(tryComplete(promise,_))

        // try complete method of promise returs a boolean, whether the promise could be completed by this result or not
        fa.onComplete(promise.tryComplete(_))
        fb.onComplete(promise.tryComplete(_))
    */
    fa.onComplete(promise.tryComplete)   // lifting is already done for us so we can avoid "_"
    fb.onComplete(promise.tryComplete)

//    fa.onComplete{
//      case Success(r) => try {
//        promise.success(r)
//      } catch {
//        case _ =>
//      }
//      case Failure(t) => try {
//        promise.failure(t)
//      } catch {
//        case _ =>
//      }
//    }
//
//    fb.onComplete{
//      case Success(r) => try{
//        promise.success(r)
//      }catch {
//        case _ =>
//      }
//      case Failure(t) => try{
//        promise.failure(t)
//      }catch {
//        case _ =>
//      }
//    }
    promise.future
  }

  // 4 - last out of the two futures
  def last[A](fa: Future[A], fb: Future[A]): Future[A] ={
    // 1 promise which both futures will try to complete
    // 2 promise which the LAST future will complete
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]

    def checkAndComplete= (result: Try[A]) =>
      if (!bothPromise.tryComplete(result))
        lastPromise.complete(result)

//    fa.onComplete(result => {
//      if (!bothPromise.tryComplete(result))
//        lastPromise.complete(result)
//    })
//
//    fb.onComplete(result => {
//      if (!bothPromise.tryComplete(result))
//        lastPromise.complete(result)
//    })

    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)

    lastPromise.future

  }

  val fast = Future {
    Thread.sleep(100)
    42
  }

  val slow = Future {
    Thread.sleep(200)
    45
  }


  first(fast,slow).foreach(println)
  last(fast,slow).foreach(println)

  first(fast,slow).foreach(f => println("FIRST: "+ f))
  last(fast,slow).foreach(l => println("LAST: "+ l))

  val t = fulfillImmediately(10)
  t.onComplete{
    case Success(t) => println("got it: " + t)
    case Failure(ex) =>
  }

  t.map(ffi => println("ffi: " + ffi))

  inSequence(fast,slow).onComplete{
    case Success(t) => println("executed: " + t)
    case Failure(ex) =>
  }
    Thread.sleep(1000)

  // retry until

  def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] =
    action()
    .filter(condition)
    .recoverWith{
      case _ => retryUntil(action, condition)
    }

  val random = new Random()
  val action = () => Future {     // here action is a zero lambda function
    Thread.sleep(100)
    val nextValue = random.nextInt(100)
    println("generated " + nextValue)
    nextValue
  }

  retryUntil(action, (x: Int) => x < 50).foreach(result => println("settled at " + result))
  Thread.sleep(10000)
}
