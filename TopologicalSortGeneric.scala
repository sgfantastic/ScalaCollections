package com.kanshu.functionalprogramming.sorting

import sun.security.provider.certpath.AdjacencyList

object TopologicalSortGeneric extends App {
  /**
   *  input - Array of pair, where second element of pair is prerequisite for the first
   *  (1,0) to complete 1 we need to complete 0 first
   *  [(1,0),(2,0),(3,1),(3,2),(4,3),(5,3)]
   *  graphical representation
   *                       0
   *                     /  \
   *                   1     2
   *                    \   /
   *                      3
   *                     / \
   *                    4   5
   *
   *
   *  output - 0,1,2,3,4,5  or 0,2,1,3,5,4
   */

  // Create a Directed Graph
  trait Graph[V] {
    def vertices: Seq[V] // the node of the graph
    def edges: Seq[(V,V)] // tuple of vertices
    def addEdges(a: V, b: V): Graph[V] // this is to add an edge to a node
    def neighbours(vertex: V): Seq[V] // neighours associated with an existing node
    def topologicalSort: Seq[V] // topological sort
  }

  object Graph{
    def apply[V](adjList: Map[V, Seq[V]]): Graph[V] = new DirectedGraph[V](adjList)
    def apply[V](): Graph[V] = new DirectedGraph[V](Map[V, Seq[V]]())
  }

  class DirectedGraph[V] (adjList: Map[V, Seq[V]]) extends Graph[V]{
    import DirectedGraph._
    override def vertices: Seq[V] = adjList.keys.toList

    override def edges: Seq[(V, V)] = adjList.flatMap { case (v, neighbours) =>
      neighbours.map((_, v))
    }.toList

    override def addEdges(a: V, b: V): Graph[V] = {
      val aNeighbour = b +: neighbours(a)
      new DirectedGraph(adjList + (a -> aNeighbour))
    }

    override def neighbours(vertex: V): Seq[V] = adjList.getOrElse(vertex, Nil)

    override def topologicalSort: Seq[V] = this.vertices.foldLeft(DfsStep[V]())((step,n) => topologicalSort(n,step)).sort

    private def topologicalSort(node: V, dfsStep: DfsStep[V]): DfsStep[V] ={
      if (dfsStep.visited contains node) dfsStep
      else {
        val preDfs = dfsStep.copy(visited = dfsStep.visited + node)
        val postDfs = this.neighbours(node).foldLeft(preDfs)((step, n) => topologicalSort(n,step))
        postDfs.copy(sort = node +: postDfs.sort)
      }
    }

  }

  object DirectedGraph {
    case class DfsStep[V](visited: Set[V] = Set[V](), sort: Seq[V] = Nil)
  }

  val g = Graph[Int]()
    .addEdges(0,1)
    .addEdges(0,2)
    .addEdges(1,3)
    .addEdges(2,3)
    .addEdges(3,4)
    .addEdges(3,5)

  println(g.topologicalSort)



}
