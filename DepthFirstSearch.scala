package com.kanshu.functionalprogramming.sorting

import com.kanshu.functionalprogramming.sorting.TopologicalSortGeneric.Graph

object DepthFirstSearch extends App{
  /**
   *                  H -- G
   *                  |    |
   *                  F - A - B - C
   *                             / \
   *                            E - D
   *  Start Node -> A
   *  DFS traversal -> A G H F B C D E
   */

  def traversalDFS[V](start: V, graph: Graph[V], visited: Seq[V] = Seq[V]()): Seq[V] ={
    if (visited contains start) visited
    else {
      graph.neighbours(start).foldLeft(visited :+ start)((visitedAll, n) => traversalDFS(n, graph, visitedAll))
    }
  }

   val graph = Graph[String]()
     .addEdges("A", "B")
     .addEdges("B", "C")
     .addEdges("C", "E")
     .addEdges("C", "D")
     .addEdges("A", "G")
     .addEdges("G", "H")
     .addEdges("H","F" )
     .addEdges("F", "A")
     .addEdges("D", "E")

  println(traversalDFS("A", graph))



}
