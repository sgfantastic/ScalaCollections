package com.kanshu.functionalprogramming.sorting

object DijktrasApp extends App {

  /**
   * Dijktras Algorithm with weighted graph
   */

  case class WeightedEdge[V](destination: V, weight: Int)

  trait Graph[V] {
    def vertices: Seq[V] // vertices is just the node of the graph
    def edges: Seq[(V,V)] // an edge is a tuple of 2 vertices
    def addEdges(a: V, b: WeightedEdge[V]): Graph[V]
    def neighbours(vertex: V): Seq[WeightedEdge[V]]
  }
  object Graph{
    def apply[V](adjList: Map[V, Seq[WeightedEdge[V]]]): Graph[V] = new WeightedGraph[V](adjList)
    def apply[V](): Graph[V] = new WeightedGraph[V](Map[V, Seq[WeightedEdge[V]]]())
  }

  class WeightedGraph[V](adjList: Map[V, Seq[WeightedEdge[V]]]) extends Graph[V] {
    override def vertices: Seq[V] = adjList.keys.toList

    override def edges: Seq[(V, V)] = adjList.flatMap{case (v, edgeList) =>
      edgeList.map(e => v -> e.destination)
    }.toList

    override def addEdges(a: V, weightedEdge: WeightedEdge[V]): Graph[V] = {
      val aNeighbour = weightedEdge +: adjList.getOrElse(a,Nil)
      new WeightedGraph(adjList + (a -> aNeighbour))
    }

    override def neighbours(vertex: V): Seq[WeightedEdge[V]] = adjList.getOrElse(vertex,Nil)
  }

  // weighted graph
  implicit val g = Graph[String]()
    .addEdges("A", WeightedEdge("B", 4))
    .addEdges("A", WeightedEdge("C", 3))
    .addEdges("B", WeightedEdge("C", 2))
    .addEdges("B", WeightedEdge("E", 1))
    .addEdges("B", WeightedEdge("G", 1))
    .addEdges("C", WeightedEdge("D", 1))
    .addEdges("D", WeightedEdge("G", 2))
    .addEdges("E", WeightedEdge("F", 1))
    .addEdges("G", WeightedEdge("F", 2))
    .addEdges("G", WeightedEdge("H", 1))
    .addEdges("F", WeightedEdge("H", 3))

  // Dijktras Algorithm
  case class Path[V](route: Seq[V]= Seq[V](), weight: Int = 0 )

  // add a path to the seq of Paths
  def addPathToRoute[V](path: Seq[Path[V]], p: Path[V], acc: Seq[Path[V]] = Seq[Path[V]]())
                       (implicit grpah: Graph[V]): Seq[Path[V]] ={
    path match {
      case Nil =>
        if (acc.map(_.route.last) contains p.route.last) acc
        else acc :+ p
      case x :: xs if x.route.last == p.route.last =>
        if (x.weight < p.weight) addPathToRoute(xs, p, acc :+ x)
        else addPathToRoute(Nil, p, acc ++ xs :+ p)
      case x :: xs => addPathToRoute(xs,p,acc :+ x)
    }
  }

  // update Route or add new path
  def updateRoute[V](neighbour: WeightedEdge[V], path: Seq[Path[V]], accPath: Seq[Path[V]])
                    (implicit graph: Graph[V]): Seq[Path[V]] ={
    path match {
      case Nil => accPath
      case x :: xs if graph.neighbours(x.route.last) contains neighbour =>
        val newAccPath = addPathToRoute(accPath,
          Path(x.route :+ neighbour.destination, x.weight + neighbour.weight))
        updateRoute(neighbour, xs,newAccPath)
      case _ :: xs => updateRoute(neighbour, xs, accPath)
    }
  }

  // add neighbours to Route
  def addNeighboursToRoute[V](allNeighbours: Seq[WeightedEdge[V]], path: Seq[Path[V]], accRoute: Seq[Path[V]])
                             (implicit graph: Graph[V]): Seq[Path[V]] ={
    allNeighbours match {
      case Nil => accRoute
      case x :: xs =>
        val newPath = updateRoute(x,path,accRoute)
        addNeighboursToRoute(xs,path,newPath)
    }
  }

  // Dijktra's shortest path algorithm
  def shortestPath[V](start: V)(implicit graph: Graph[V]): Seq[Path[V]] ={
    val startNeighbour = graph.neighbours(start)
    val startPath = Path(Seq(start))
    def shortestPathHelper(neighbour: Seq[WeightedEdge[V]], path: Seq[Path[V]]): Seq[Path[V]] ={
      neighbour match {
        case Nil => path
        case n =>
          val newNeighbours = n.flatMap(t => graph.neighbours(t.destination))
          shortestPathHelper(newNeighbours, addNeighboursToRoute(n,path,path))
      }
    }
    shortestPathHelper(startNeighbour, Seq(startPath))
  }

  val sp = shortestPath("A")
  println(sp)
  println(sp.filter(_.route.last == "H"))
  println(sp.filter(_.route.last == "F"))

}
