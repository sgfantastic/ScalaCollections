package com.kanshu.datastructures

import scala.util.Random

object SudokuPuzzleBoard extends App{
  /**
  Sudoku puzzle board - 9x9 matrix
      0  1  2  3  4  5  6  7  8
      --------------------------
    0|5, 7, 4, 6, 1, 8, 9, 2, 3
    1|3, 9, 1, 4, 2, 7, 6, 5, 8
    2|2, 8, 6, 3, 9, 5, 1, 4, 7
    3|7, 4, 9, 5, 8, 3, 2, 1, 6
    4|6, 1, 3, 2, 7, 4, 8, 9, 5
    5|8, 5, 2, 1, 6, 9, 3, 7, 4
    6|9, 6, 7, 8, 5, 1, 4, 3, 2
    7|4, 2, 5, 9, 3, 6, 7, 8, 1
    8|1, 3, 8, 7, 4, 2, 5, 6, 9

  Block - 3x3 matrix
      0  1  2
      -------
    0|5, 7, 4
    1|3, 9, 1
    2|2, 8, 6

   */

  val sudokuGrid = 9
  val default = Seq(1,2,3,4,5,6,7,8,9)
  val b = Random.shuffle(default)
//  println(b)

  val defaultSudokuBoard = Array.fill(sudokuGrid, sudokuGrid)(-1)  // Seq[Int]

  implicit class SudokuConstraint(seq: Seq[Int]) {
    val newSeq = seq.grouped(sudokuGrid).foldLeft(Seq[Seq[Int]]())((acc, a) => acc :+ a)

    // return the values for the box for given index
    def getBox(idx: Int): Seq[Int] = {
      val(m,d) = (idx%sudokuGrid, idx/sudokuGrid)
      m match {
        case m1 if m1 <= 2 =>
          d match {
            case d1 if d1 <=2 => (0 to 2).flatMap(r => (0 to 2).map(c => newSeq(r)(c)))
            case d1 if 2 < d1 & d1 < 6 => (3 to 5).flatMap(r => (0 to 2).map(c => newSeq(r)(c)))
            case d1 if 5 < d1 => (6 to 8).flatMap(r => (0 to 2).map(c => newSeq(r)(c)))
          }

        case m1 if 2 < m1 & m1 < 6 =>
          d match {
            case d1 if d1 <=2 => (0 to 2).flatMap(r => (3 to 5).map(c => newSeq(r)(c)))
            case d1 if 2 < d1 & d1 < 6 => (3 to 5).flatMap(r => (3 to 5).map(c => newSeq(r)(c)))
            case d1 if 5 < d1 => (6 to 8).flatMap(r => (3 to 5).map(c => newSeq(r)(c)))
          }

        case m1 if m1 > 5 =>
          d match {
            case d1 if d1 <=2 => (0 to 2).flatMap(r => (6 to 8).map(c => newSeq(r)(c)))
            case d1 if 2 < d1 & d1 < 6 => (3 to 5).flatMap(r => (6 to 8).map(c => newSeq(r)(c)))
            case d1 if 5 < d1 => (6 to 8).flatMap(r => (6 to 8).map(c => newSeq(r)(c)))
          }

        case _ => Nil
      }
    }

    // return the values for row and column for the given index
    def getMatrixData(idx:Int): Seq[Int] = {
      val (c,r) = (idx%sudokuGrid, idx/sudokuGrid)
      (0 to sudokuGrid-1).map(cl => newSeq(cl)(c)) ++ (0 to sudokuGrid-1).map(rw => newSeq(r)(rw))
    }
  }

  def createSudoku(tmp: Seq[Int], accList: Seq[Int]=Nil): Seq[Int] ={
    if (accList.length == sudokuGrid*sudokuGrid) accList
    else {
      val idx = accList.length
      val(_, rest) = defaultSudokuBoard.flatten.splitAt(accList.length)
      val newSeq = accList ++ rest
      val filterConstraints = (newSeq.getMatrixData(idx) ++ newSeq.getBox(idx)).toSet
      val newTmp = tmp.filterNot(filterConstraints.contains)
      val randomHead = Random.shuffle(newTmp).headOption.getOrElse(-1)
      createSudoku(tmp, accList :+ randomHead)
    }
  }

//  createSudoku(default).grouped(sudokuGrid).foreach(println)

  def generateSudokuBoard(seq: Seq[Int]): Seq[Int] = {
    val sudoku = createSudoku(default,seq)
    if(sudoku contains -1 ) generateSudokuBoard(Seq(Random.nextInt(sudokuGrid-1) + 1))
    else sudoku
  }

  println("******sudoku board****")
  generateSudokuBoard(b).grouped(sudokuGrid).foreach(println)
}
