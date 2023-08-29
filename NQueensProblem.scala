package com.kanshu.datastructures

object NQueensProblem extends App {
  /**
   * The N Queens problem is a classic puzzle that involves placing N chess queens on an
   * N×N chessboard in such a way that no two queens can be in the
   * same row, column, or diagonal.
   * Backtracking is a common technique used to solve this problem.
   * Time Complexity: O(N^N)
   * Space Complexity: O(N^2)
   *
   * eg. for no queens = 4 and board size 4 x 4
   * the no possible solutions = 2
   *
   * solution - 1
   *   . Q . .
   *   . . . Q
   *   Q . . .
   *   . . Q .
   *
   *   solution - 2
   *   . . Q .
   *   Q . . .
   *   . . . Q
   *   . Q . .
   */

  case class Queen(row: Int, column: Int)

  private def isSafe(queen: Queen, queens: Seq[Queen]): Boolean =
    queens.forall(q => !inCheck(queen,q))

  private def inCheck(q1: Queen, q2: Queen): Boolean =
    q1.row == q2.row || q1.column == q2.column || (q1.row - q2.row).abs == (q1.column-q2.column).abs

private def solveNQueens(n: Int): Seq[Seq[Queen]] = {
  def placeQueens(k: Int): Seq[Seq[Queen]] = {
    if(k == 0) Seq(Nil)
    else {
      placeQueens(k - 1).flatMap(queens => {
        (1 to n).flatMap(clm =>{
          val queen = Queen(k,clm)
          if(isSafe(queen, queens)) Seq(queens :+ queen)
          else Nil
        })
      })
    }
  }
  placeQueens(n)
}

  private def printSolution(solutions: Seq[Queen]): Unit ={
    val board = Array.fill(solutions.length, solutions.length)(".")
    solutions.foreach{case Queen(r,c) => board(r-1)(c-1) = "Q"}
    board.foreach(row => println(row.mkString(" ")))
    println()
  }
  val n = 4
  val solutions = solveNQueens(4)
  solutions.foreach(println)
  println(s"no of solutions - ${solutions.length}")
  solutions.foreach(printSolution)

}
