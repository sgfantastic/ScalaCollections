package com.kanshu.functionalprogramming.sorting

import com.kanshu.functionalprogramming.sorting.TopologicalSortGeneric.Graph
import sun.reflect.generics.visitor.Visitor

object BreadthFirstSearch extends App{
  /**
   *
   *                  H -- G
   *                  |    |
   *                  F - A - B - C
   *                             / \
   *                            E - D
   *  Start Node -> A
   *  BFS traversal -> A G B H C F D E
   */

    // add a seq to a seq
  def addSeqToSeq[V](seqTo: Seq[V])(seqFrom: Seq[V]): Seq[V] ={
    seqFrom.foldLeft(seqTo)((st, t) =>{
      if (st contains t) st
      else st :+ t
    })
  }

  // get all the associated nodes
  def getChild[V](graph: Graph[V])(node: V): Seq[V] = graph.neighbours(node)

  // filter seq from a seq
  def filterFromSeq[V](seqTo: Seq[V])(seqFrom: Seq[V]) : Seq[V] =
    seqTo.foldLeft(Seq[V]())((seq, v) => {
      if (seqFrom contains v) seq
      else seq :+ v
    })

  def breadthFirstSearch[V](start: Seq[V], graph: Graph[V], visited: Seq[V] = Seq[V]()) : Seq[V] = {
    val getNext = getChild(graph)(_)
    val newVisited = addSeqToSeq(visited)(start)
    val next = start.flatMap(getNext(_))
    if (next.isEmpty) newVisited
    else {
      val newVisited1 = addSeqToSeq(newVisited)(next)
      breadthFirstSearch(filterFromSeq(next)(visited),graph,newVisited1)
    }
  }

  val graph = Graph[String]()
    .addEdges("A", "B")
    .addEdges("B", "C")
    .addEdges("C", "E")
    .addEdges("C", "D")
    .addEdges("A", "G")
    .addEdges("G", "H")
    .addEdges("H", "F")
    .addEdges("F", "A")
    .addEdges("D", "E")

  println(graph.neighbours("A").flatMap(graph.neighbours(_)))
  println(bfs(Seq("A"), graph))

}
