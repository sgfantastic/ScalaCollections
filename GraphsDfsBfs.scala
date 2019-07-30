package com.kanshu.scala

import scala.annotation.tailrec

/* Graph
  1 -- 2
  |    |
  4 -- 3
 */

object GraphApp extends App{

  var intGraph = new Graph[Int, Int]
  intGraph.g = Map(1 -> List(2,4), 2 -> List(1,3), 3 -> List(2,4), 4 -> List(1,3))

  println("****BFS******")
  println(intGraph.BFS(1))
  println(intGraph.BFS(2))
  println(intGraph.BFS(3))
  println(intGraph.BFS(4))

  println("****DFS******")
  println(intGraph.DFS(1))
  println(intGraph.DFS(2))
  println(intGraph.DFS(3))
  println(intGraph.DFS(4))

}

class Graph[+T, B <: T] {
  type GraphMap[B] = Map[B,List[B]]
  var g: GraphMap[B] = Map()

// Breadth First Search
  def BFS(start: B): List[List[B]] ={
    @tailrec
     def BFSHelper(elem: List[B], visited: List[List[B]]): List[List[B]] = {
       val newNeighbour = elem.flatMap(g(_)).filterNot(visited.flatten.contains).distinct
       if (newNeighbour.isEmpty) visited
       else BFSHelper(newNeighbour, newNeighbour :: visited)
     }
    BFSHelper(List(start), List(List(start))).reverse
  }
  // Depth first Search

  def DFS(start: B): List[B] = {
    def DFSHelper(v: B, visited: List[B]): List[B] = {
      if (visited.contains(v)) visited
      else {
        val neighbours: List[B] = g(v) filterNot visited.contains
        neighbours.foldLeft(v :: visited)((b,a)=>DFSHelper(a,b))
      }
    }
    DFSHelper(start, List()).reverse
  }
}
