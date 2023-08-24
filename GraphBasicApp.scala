package com.kanshu.datastructures

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ClosedShape, Materializer}
import akka.stream.scaladsl.{Balance, Broadcast, Flow, GraphDSL, Merge, RunnableGraph, Sink, Source, Zip}

object GraphBasicApp extends App{

    implicit val system = ActorSystem("GraphBasics")
    implicit val materializer = Materializer.matFromSystem(system)

    val input = Source(1 to 1000)
    val incrementer = Flow[Int].map(_ + 1) // hard computation
    val multiplier = Flow[Int].map(_ * 10) // hard computation
    val output = Sink.foreach[(Int, Int)](println)

    val graph = RunnableGraph.fromGraph(
      GraphDSL.create(){ implicit builder: GraphDSL.Builder[NotUsed] => // builder = Mutable Data structure
        import GraphDSL.Implicits._

        val broadcast = builder.add(Broadcast[Int](2)) // fan-out operator
        val zip = builder.add(Zip[Int, Int]) // fan-in operator

        input ~> broadcast

        broadcast.out(0) ~> incrementer ~> zip.in0
        broadcast.out(1) ~> multiplier ~> zip.in1

        zip.out ~> output
        ClosedShape // freeze the builder shape
      }
    )

    //  graph.run()

    /**
     * exercise 1: feed a source into 2 sinks at the same time(hint: use a broadcast
     */

    val firstSink = Sink.foreach[Int](x => println(s"First sink: $x"))
    val secondSink = Sink.foreach[Int](x => println(s"Second sink: $x"))

    val sourceToSinkGraph = RunnableGraph.fromGraph(
      GraphDSL.create(){implicit builder =>
        import GraphDSL.Implicits._

        val broadcast = builder.add(Broadcast[Int](2))
        input ~> broadcast ~> firstSink  // implicit port numbering
        broadcast ~> secondSink
        //      broadcast.out(0) ~> firstSink
        //      broadcast.out(1) ~> secondSink

        ClosedShape
      }
    )

    /**
     * exercise2: balance input
     */

    import scala.concurrent.duration._
    val fastSource = input.throttle(5, 1 second)
    val slowSource = input.throttle(2, 1 second)

    val sink1 = Sink.fold[Int,Int](0)((count, _) => {
      println(s"Sink 1 number of element: $count")
      count + 1
    })
    val sink2 = Sink.fold[Int, Int](0)((count, _) => {
      println(s"Sink 2 number of element: $count")
      count + 1
    })

    val balancedGraph = RunnableGraph.fromGraph(
      GraphDSL.create(){implicit builder =>
        import GraphDSL.Implicits._

        val merge = builder.add(Merge[Int](2))
        val balance = builder.add(Balance[Int](2))

        fastSource ~> merge ~> balance ~> sink1
        fastSource ~> merge
        balance ~> sink2

        ClosedShape
      }
    )

    balancedGraph.run()

  }
