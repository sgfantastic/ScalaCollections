package com.kanshu.datastructures

object GraphGeneric extends App{
  /**
   *  input - Array of pair, where second element of pair is prerequisite for the first
   *  (1,0) to complete 1 we need to complete 0 first
   *  [(1,0),(2,0),(3,1),(3,2),(4,3),(5,3)]
   *  graphical representation - Directed Graph
   *                       0
   *                     /  \
   *                   1     2
   *                    \   /
   *                      3
   *                     / \
   *                    4   5
   *
   *
   *  bfs - 0, 2, 1, 3, 5, 4 time complexity - O(V + E) space complexity - O(V)
   *  dfs - 1, 4, 5, 3, 2, 0
   */

  trait Graph[V]{
    def vertices: Seq[V] // node of a graph
    def edges: Seq[(V,V)] // tuple of vertices
    def addEdges(a: V, b: V): Graph[V] // add new edge
    def neighbours(vertex: V): Seq[V] // neighbours associated wit the existing node
  }

  object Graph{
    def apply[V](adjList: Map[V, Seq[V]]): Graph[V] = new DirectedGraph[V](adjList)
    def apply[V]():Graph[V] = new DirectedGraph[V](Map.empty[V, Seq[V]])
  }

  class DirectedGraph[V](adjList: Map[V, Seq[V]]) extends Graph[V] {
    override def vertices: Seq[V] = adjList.keys.toSeq

    override def edges: Seq[(V, V)] = adjList
      .flatMap{case(v,neighbour) => neighbour.map((_,v))}.toSeq

    override def addEdges(a: V, b: V): Graph[V] = {
      val aNeighbour = b +: neighbours(a)
      new DirectedGraph(adjList + (a -> aNeighbour))
    }

    override def neighbours(vertex: V): Seq[V] = adjList.getOrElse(vertex, Nil)
  }

  val g = Graph[Int]()
    .addEdges(0,1)
    .addEdges(0,2)
    .addEdges(1,3)
    .addEdges(2,3)
    .addEdges(3,4)
    .addEdges(3,5)

  private def bfs[V](graph: Graph[V], start: V): Seq[V] = {
    def bfsHelper(queue: Seq[V], visited: Set[V] = Set[V](), acc: Seq[V] = Nil): Seq[V] ={
      queue match {
        case Nil => acc
        case head :: tail =>
          if(visited contains head) bfsHelper(tail, visited, acc)
          else {
            val neighbours = graph.neighbours(head).filterNot(visited.contains)
            bfsHelper(tail ++ neighbours, visited + head, acc :+ head)
          }
      }
    }
    bfsHelper(Seq(start))
  }

  println(bfs(g, 0))








}

