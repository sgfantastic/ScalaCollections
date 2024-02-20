import akka.actor.{Actor, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import org.apache.spark.sql.{DataFrame, SparkSession}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object AkkaBankApp extends App{

  val spark = SparkSession.builder()
    .appName("SparkTest2App")
    .master("local[*]")
    .getOrCreate()
  import spark.implicits._
  object DataStore{

    val sales = {
      Thread.sleep(5000)
      Seq(
        (1, 2 ,3),
        (2, 2, 4),
        (3, 2, 5)
      ).toDF("id", "pid", "cost")
    }

    val dept = Seq(
      (1, "sports"),
      (2, "health"),
      (3, "extra")
    ).toDF("pid", "name")
  }

  implicit val system = ActorSystem("AkkaBankApp")
  import system.dispatcher

  case class Credit(amount: Int)
  case class Debit(amount: Int)
  case object CheckBalance
  case object GetDept
  case object GetSales

  class BankActor extends Actor {
    var balance: Int = 0
    override def receive: Receive = {
      case Credit(amount) =>
        balance += amount
      case Debit(amount) =>
        balance -= amount
      case CheckBalance =>
       sender() ! balance
      case GetDept =>
        sender() ! DataStore.dept
      case GetSales =>
        sender() ! DataStore.sales
    }
  }
  implicit val defaultTimeout = Timeout(5 seconds )
  val bankActor = system.actorOf(Props[BankActor], "bankActor")

  bankActor ! Credit(100)

  bankActor ! Credit(100)
  (bankActor ? CheckBalance).mapTo[Int].onComplete {
    case Success(value) => println(s"the balance is $value")
    case Failure(ex) => println(s"Cannot retrieve balance - $ex")
  }
  bankActor ! Credit(100)
//  bankActor ! CheckBalance
  bankActor ! Credit(100)

  (bankActor ? CheckBalance).mapTo[Int].onComplete {
    case Success(value) => println(s"the balance is $value")
    case Failure(ex) => println(s"Cannot retrieve balance - $ex")
  }

  bankActor ! Debit(100)
  (bankActor ? CheckBalance).mapTo[Int].onComplete{
    case Success(value) =>println(s"the balance is $value")
    case Failure(ex) => println(s"Cannot retrieve balance - $ex")
  }




//    (bankActor ? GetDept).mapTo[DataFrame].onComplete{
//    case Success(df) => df.show(10, false)
//    case Failure(e: Throwable) => e.printStackTrace()
//    }
//
//
//  (bankActor ? GetSales).mapTo[DataFrame].onComplete {
//    case Success(df) => df.show(10, false)l
//    case Failure(e: Throwable) => e.printStackTrace()
//  }

  for {
    dept <- (bankActor ? GetDept).mapTo[DataFrame]
    sales <- (bankActor ? GetSales).mapTo[DataFrame]
  } yield {
    dept.show(10, false)
    sales.show(10, false)
  }

  val sales = (bankActor ? GetSales).mapTo[DataFrame]

  val dept = (bankActor ? GetDept).mapTo[DataFrame]

  val sales1 = Await.result(sales, 2 seconds)
  val dept1 = Await.result(dept, 5 seconds)

  sales1.show(10, false)
  dept1.show(10, false)

  system.terminate()
}
