package com.kanshu.datastructures

object KafkAkkaConsumerApp {

  import akka.Done
  import akka.actor.ActorSystem
  import akka.kafka.{ConsumerSettings, Subscriptions}
  import akka.kafka.scaladsl.Consumer
  import akka.stream.{ActorMaterializer, Materializer, SinkShape}
  import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, RunnableGraph, Sink, Source}
  import org.apache.kafka.clients.consumer.{ConsumerConfig, ConsumerRecord}
  import org.apache.kafka.common.serialization.StringDeserializer

  import scala.concurrent.duration._
  import scala.concurrent.{Await, Future}

  object KafkaAkkaConsumerApp extends App{

    implicit val system: ActorSystem = ActorSystem("KafkaConsumerExample")
    implicit val materializer: Materializer = Materializer.matFromSystem(system)

    // Define Kafka consumer settings
    val consumerSettings: ConsumerSettings[String, String] =
      ConsumerSettings(system, new StringDeserializer, new StringDeserializer)
        .withBootstrapServers("<broker>") // Kafka broker address
        .withGroupId("my-group") // Consumer group
        .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest") // Start from the beginning of the topic

    // Define the Kafka topic you want to consume from
    val kafkaTopic = "<topic>"

    // Create a Kafka source
    val kafkaSource: Source[ConsumerRecord[String, String], Consumer.Control] =
      Consumer.plainSource(consumerSettings, Subscriptions.topics(kafkaTopic))

    val processFlow: Flow[ConsumerRecord[String, String], ConsumerRecord[String, String], _] =
      Flow[ConsumerRecord[String, String]].map { record =>
        println(s"Received value  message: ${record.value()}")
        record
      }

    // Sink: Just consuming the message and materializing a future
    val sink: Sink[ConsumerRecord[String, String], Future[Done]] =
      Sink.foreach(_ => ())

    // Create a RunnableGraph connecting source, flow, and sink
    val graph: RunnableGraph[Consumer.Control] = kafkaSource.via(processFlow).to(sink)

    val control: Consumer.Control = graph.run()

    private val pathToDispatchers = "/user/*"
    Runtime.getRuntime.addShutdownHook(new Thread() {
      () => {
        // Gracefully stop the Akka ActorSystem
        val termination = system.terminate()
        control.shutdown()
        system.actorSelection(pathToDispatchers) ! akka.actor.PoisonPill
        Await.result(termination, 60.seconds)
      }
    })


}
