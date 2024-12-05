package com.pg.bigdata.neighborhoodanalytics.aoc.fp

import cats.effect.IO

import scala.annotation.tailrec

object Day4 extends Exercise(2024, 4) {

  private val xmas = "XMAS"
  private val samx = "SAMX"
  private val mas = "MAS"
  private val sam = "SAM"

  private type Matrix = List[List[Char]]
  private type MatricesPerXLines = List[Matrix]
  private type Matrices = List[MatricesPerXLines]

  private def makeXLiners(input: List[String], lines: Int): List[List[String]] = {
    input.sliding(lines).toList
  }

  private def makeXxXMatrices(xLiner: List[String], size: Int): MatricesPerXLines = {
      xLiner.transpose.sliding(size).toList
  }

  private def findHorizontal(matrix: Matrix, firstFour: Boolean): Int = {

    @tailrec
    def helper(matrix: Matrix, acc: Int): Int = matrix match {
      case Nil => acc
      case row :: tail => if (row.mkString("") == xmas || row.mkString("") == samx) helper(tail, acc + 1) else helper(tail, acc)
    }

    if (firstFour) helper(matrix, 0) // If it's not the first four, check only the last row to avoid double counting
    else List(matrix(3)(0), matrix(3)(1), matrix(3)(2), matrix(3)(3)).mkString("") match {
      case `xmas` | `samx` => 1
      case _ => 0
    }
  }

  private def findVertical(matrix: Matrix, firstColumn: Boolean): Int = {

    @tailrec
    def helper(matrix: Matrix, column: Int, acc: Int): Int = column match {
      case 4 => acc
      case _ => List(matrix(0)(column), matrix(1)(column), matrix(2)(column), matrix(3)(column)).mkString("") match {
       case `xmas` | `samx` => helper(matrix, column + 1, acc + 1)
       case _ => helper(matrix, column + 1, acc)
      }
    }

    if (firstColumn) helper(matrix, 0, 0) // If it's not the first column, check only the last column to avoid double counting
    else List(matrix(0)(3), matrix(1)(3), matrix(2)(3), matrix(3)(3)).mkString("") match {
      case `xmas` | `samx` => 1
      case _ => 0
    }
  }

  private def findDiagonal(matrix: Matrix): Int = {
    val diagonal1 = List(matrix(0)(0), matrix(1)(1), matrix(2)(2), matrix(3)(3)).mkString("")
    val diagonal2 = List(matrix(0)(3), matrix(1)(2), matrix(2)(1), matrix(3)(0)).mkString("")

    val firstDiagonal = if (diagonal1 == xmas || diagonal1 == samx) 1 else 0
    val secondDiagonal = if (diagonal2 == xmas || diagonal2 == samx) 1 else 0
    firstDiagonal + secondDiagonal
  }

  private def findTotalForEachCase(matricesPerFourLines: MatricesPerXLines, firstFour: Boolean): Int = {

    @tailrec
    def helper(matrices: MatricesPerXLines, acc: Int, firstColumn: Boolean): Int = matrices match {
      case Nil => acc
      case matrix :: tail =>
        val transposed = matrix.transpose
        val horizontal = findHorizontal(transposed, firstFour)
        val vertical = findVertical(transposed, firstColumn)
        val diagonal = findDiagonal(transposed)
        helper(tail, acc + horizontal + vertical + diagonal, false)
    }

    helper(matricesPerFourLines, 0, true)
  }

  private def findTotal(matricesPerEachFourLines: Matrices): Int = {
    val firstFour = matricesPerEachFourLines.head
    val totalForFirstFour = findTotalForEachCase(firstFour, true)
    val totalForTheRest = matricesPerEachFourLines.tail.map { matrices => findTotalForEachCase(matrices, false) }.sum
    totalForFirstFour + totalForTheRest
  }

  override def part1(input: List[String]): IO[String] = {
    for {
      fourLiners <- IO.pure(makeXLiners(input, 4))
      listOfMatrices = fourLiners.map { fourLiner => makeXxXMatrices(fourLiner, 4) }
      totals = findTotal(listOfMatrices)
      _ = println(totals)
    } yield totals.toString
  }

  private def findBothDiagonals(matrix: Matrix): Int = {
    val diagonal1 = List(matrix(0)(0), matrix(1)(1), matrix(2)(2)).mkString("")
    val diagonal2 = List(matrix(0)(2), matrix(1)(1), matrix(2)(0)).mkString("")

    val firstDiagonal = if (diagonal1 == mas || diagonal1 == sam) 1 else 0
    val secondDiagonal = if (diagonal2 == mas || diagonal2 == sam) 1 else 0
    if (firstDiagonal + secondDiagonal == 2) 1 else 0
  }

  private def findTotalXs(matricesPerEachThreeLines: MatricesPerXLines): Int = {

    @tailrec
    def helper(matrices: MatricesPerXLines, acc: Int): Int = matrices match {
      case Nil => acc
      case matrix :: tail =>
        val diagonal = findBothDiagonals(matrix)
        helper(tail, acc + diagonal)
    }

    helper(matricesPerEachThreeLines, 0)
  }

  override def part2(input: List[String]): IO[String] = {
    for {
      threeLiners <- IO.pure(makeXLiners(input, 3))
      listOfMatrices = threeLiners.map { threeLiner => makeXxXMatrices(threeLiner, 3) }
      totals = listOfMatrices.map { matrices => findTotalXs(matrices) }.sum
      _ = println(totals)
    } yield totals.toString
  }

}
