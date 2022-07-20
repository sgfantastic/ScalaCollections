package com.kanshu.functionalprogramming.sorting

object DijktrasApp extends App {

  /**
   * Dijktras Algorithm with weighted graph
   */

  case class WeightedEdge[V](destination: V, weight: Int)

  trait Graph[V] {
    def vertices: Seq[V] // vertices is just the node of the graph
    def edges: Seq[(V,V)] // an edge is a tuple of 2 vertices
    def addEdge(a: V, weightedEdge: WeightedEdge[V]): Graph[V]
    def neighbours(vertex: V): Seq[WeightedEdge[V]]
  }

  object Graph{
    def apply[V](adjList: Map[V, Seq[WeightedEdge[V]]]): Graph[V] = new WeightedGraph[V](adjList)
    def apply[V](): Graph[V] = new WeightedGraph(Map[V, Seq[WeightedEdge[V]]]())
  }

  // Weighted Graph
  class WeightedGraph[V](adjList: Map[V, Seq[WeightedEdge[V]]]) extends Graph[V]{
    override def vertices: Seq[V] = adjList.keys.toList

    override def edges: Seq[(V, V)] = adjList.flatMap{case (v, edgeList) =>
      edgeList.map(e => v -> e.destination)
    }.toList

    override def addEdge(a: V, weightedEdge: WeightedEdge[V]): Graph[V] = {
      val aNeighbour = weightedEdge +: adjList.getOrElse(a,Nil)
      new WeightedGraph(adjList + (a -> aNeighbour))
    }

    override def neighbours(vertex: V): Seq[WeightedEdge[V]] = adjList.getOrElse(vertex, Nil)
  }

  val g = Graph[String]()
    .addEdge("A", WeightedEdge("B", 4))
    .addEdge("A", WeightedEdge("C", 3))
    .addEdge("B", WeightedEdge("C", 2))
    .addEdge("B", WeightedEdge("E", 1))
    .addEdge("B", WeightedEdge("G", 1))
    .addEdge("C", WeightedEdge("D", 1))
    .addEdge("D", WeightedEdge("G", 2))
    .addEdge("E", WeightedEdge("F", 2))
    .addEdge("G", WeightedEdge("F", 2))
    .addEdge("G", WeightedEdge("H", 1))
    .addEdge("F", WeightedEdge("H", 3))

  // Dijktra's Algorithm

  case class Path[V](route: Seq[V] = Seq[V](), weight: Int = 0)

  // add a path to the seq of Paths
  def addToPath[V](path: Seq[Path[V]], p: Path[V], acc: Seq[Path[V]]= Seq[Path[V]]()): Seq[Path[V]] = {
    path match {
      case Nil =>
        val dest = acc.map(_.route.last)
        if (dest contains p.route.last) acc
        else acc :+ p
      case x :: xs if x.route.last == p.route.last =>
        if (x.weight < p.weight) addToPath(xs, p, acc :+ x)
        else addToPath(Nil, p, acc ++ xs :+ p)
      case x :: xs => addToPath(xs, p, acc :+ x)
    }
  }

  def shortestPath[V](start: V, graph: Graph[V]): Seq[Path[V]] = {
    val startNeighbour = graph.neighbours(start)
    val newPath = startNeighbour.foldLeft(Seq[Path[V]]())((path, n) =>
      path :+ Path(Seq(start, n.destination), n.weight)
    )

    val newNeighbour = startNeighbour.foldLeft(Seq[WeightedEdge[V]]())((nn, n) =>
      nn ++ graph.neighbours(n.destination)
    )

    def addNeighboursToPath(neighbour: Seq[WeightedEdge[V]] ,
                            path: Seq[Path[V]] ): Seq[Path[V]] ={
      neighbour match {
        case Nil => path
        case n =>
          val newNeighbours = n.flatMap(node => graph.neighbours(node.destination))
          def addNeighbour(nei: Seq[WeightedEdge[V]], accRoute: Seq[Path[V]]): Seq[Path[V]] ={
            nei match {
              case Nil => accRoute
              case x :: xs =>
                val newPath = updatePath(x,path,accRoute)
                addNeighbour(xs, newPath)
            }
          }

          def updatePath(n1: WeightedEdge[V], path: Seq[Path[V]], accPath: Seq[Path[V]])
          : Seq[Path[V]] ={
            path match {
              case Nil => accPath
              case x :: xs if graph.neighbours(x.route.last) contains n1=>
                val newAccPath = addToPath(accPath, Path(x.route :+ n1.destination, x.weight + n1.weight))
                updatePath(n1,xs,newAccPath)
              case _ :: xs => updatePath(n1,xs, accPath)
            }
          }
          addNeighboursToPath(newNeighbours, addNeighbour(n,path))
      }
    }
    addNeighboursToPath(newNeighbour, newPath)
  }

  val sp = shortestPath("A", g)
  println(sp)
  println(sp.filter(_.route.last == "H"))
  println(sp.filter(_.route.last == "F"))

}
