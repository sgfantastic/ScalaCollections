package com.kanshu.datastructures

import akka.actor.ActorSystem
import akka.stream.{Materializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Sink, Source}
import org.apache.spark.streaming.Seconds

import scala.concurrent.duration._

object BackPressureApp extends App{
  // One fo the fundamental features of reactive streams
  // based on few ideas -
  //     Elements flow as response to demand from consumers
  //     Consumers are actually the ones who trigger the flow of elements through a stream
  //     If I have a simple stream composed of source , flow and a sink, elements don't flow through the stream unless there is a demand from that sink
  //     Sink will issue demand from upstream and flow in between also issue demand from the source and after that the element will start flowing

  // fast consumer : all is well
  // slow consumer : problem
  // if the sink is slow - it will send a signal upstream to slow down
  // if the flow unable to comply it will send the signal upstream to slow down
  // if the consumer sends more demand then the rate of the stream may increase again - this protocol is knows as back pressure and its transparent to programmers

  implicit val actoSystem = ActorSystem("BackPressure")
  implicit val materizer = Materializer.matFromSystem(actoSystem)

  val fastSource = Source(1 to 1000)
  val slowSink = Sink.foreach[Int]{x =>
    // simulate long processing
    Thread.sleep(1000)
    println(s"Sink: $x")
  }

  //  fastSource.to(slowSink).run()  // fusing ?

  // this is not back pressure

  //  fastSource.async.to(slowSink).run()  // here we have back pressure

  val simpleFlow = Flow[Int].map{x =>
    println(s"Incoming: $x")
    x + 1
  }

  //  fastSource.async.via(simpleFlow).async.to(slowSink).run()
  /*
  reaction to backpressure (in order):
  - try to slow down if possible
  - buffer elements until there is more demand
  - drop down elements from the buffer if it overflows
  - tear down/kill the whole stream(failure)
   */

  val bufferedFlow = simpleFlow.buffer(10, overflowStrategy = OverflowStrategy.dropHead) // dropHead - drops the oldest element form the buffer when a new element arrives
  fastSource.async
    .via(bufferedFlow).async
    .to(slowSink)
  //    .run()

  /*
  overflow strategies:
   - drop head = oldest
   - drop tail = newest
   - drop new = exact element to be added = keeps the buffer
   - drop the entire buffer
   - backpressure signal
   - fail
   */

  // throttling
  fastSource.throttle(2, 1 second).runWith(Sink.foreach(println))
//  actoSystem.terminate()
