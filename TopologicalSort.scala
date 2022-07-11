package com.kanshu.functionalprogramming.sorting

import scala.annotation.tailrec

object TopologicalSort extends App{
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
   *  output - 0,1,2,3,5,4  or 0,2,1,3,5,4
   */

  // input Array
//  val lst = Seq((1,0),(2,0),(3,1),(3,2),(4,3),(5,3))
  val lst = Seq((1,0),(2,0),(3,1),(3,2),(4,3),(5,3)).reverse

  //create adjacency List from input
  //Key - dependency value = Seq of nodes

  val mapAdjList = lst.foldLeft(Map[Int, Seq[Int]]())((mp,l) => {
    if(!mp.contains(l._2)) mp + (l._2 -> Seq(l._1)) // l(1,0) l._1 = 1, l._2 =0 (0 -> Seq(1))
    else {
      val tmpLst = mp.get(l._2).map(_ :+ l._1).get
      mp.filterNot(_._1 == l._2) + (l._2 -> tmpLst)
    }
  })
  println(mapAdjList)

  // topological sorting
  // Map(0 -> List(1, 2), 1 -> List(3), 2 -> List(3), 3 -> List(4, 5))
  // map.head(0, Seq(1,2)) sort= ()   temp=0,1,2
  // map.head(1, Seq(3))   sort= ()   temp=0,1,2,3
  // map.head(2, Seq(3))   sort= ()   temp=0,1,2,3
  // map.head(4, Seq(4,5))   sort=(4,5,3)   temp=0,1,2,3
  // sort ++ temp.reverse = (4,5,3,2,1,0)

  // add Seq to seq
   def addSeqToSeq(seqTo: Seq[Int])(seqFrom: Seq[Int]): Seq[Int] ={
     seqFrom.foldLeft(seqTo)((st, t) =>{
       if (st contains t) st
       else st :+ t
     })
   }
  // add value to seq

  def addToSeq(seq:Seq[Int])(value: Int): Seq[Int] ={
    if (seq contains value) seq
    else seq :+ value
  }

  // topological sort

  def topologicalSorting(mapAdjList: Map[Int, Seq[Int]], sort: Seq[Int] = Seq[Int](), tempList: Seq[Int] = Seq[Int]()): Seq[Int] ={
    (mapAdjList, tempList) match {
      case (x, _) if x.isEmpty => sort
      case (m, tmp) =>
        val mapHead = m.head
        val key = mapHead._1 // 0
        val value = mapHead._2  // (1,2)
        // check if there is any outgoing node
      val checkDegree = value.map(m.get(_).isEmpty) // (1,2)

        // add the values to tmp or sort
      val (tmpLst, tmpSort) = value.foldLeft(Seq[Int](), Seq[Int]())((allList, vl) => {
        val (tmp, srt) = allList
        (tmp,srt) match {
          case (tp, st) if !m.get(vl).isDefined && !st.contains(vl) => (tp, st:+ vl)
          case (tp,st) if !tp.contains(vl) => (tp :+ vl, st)
        }
      })
        // add the key to tmp or sort
        val (tmpSort1, tmp1) = (tmpSort, tmp, checkDegree) match {
          case (_,_, f) if f contains false => (tmpSort, addToSeq(tmp)(key))
          case (Nil,_,_) => (Nil, addToSeq(tmp)(key))
          case (tpSrt, tplst, _) => (addSeqToSeq(tpSrt :+ key)(tplst.reverse), Nil)
        }

        val newSort = addSeqToSeq(sort)(tmpSort1)
        val newTmp = addSeqToSeq(tmp1)(tmpLst)

        topologicalSorting(m.filterNot(_._1 == key), newSort,newTmp)
    }
  }


  println(topologicalSorting(mapAdjList))

}
