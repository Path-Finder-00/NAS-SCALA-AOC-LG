package com.pg.bigdata.neighborhoodanalytics.aoc.fp

import cats.effect.IO

import scala.annotation.tailrec
import scala.math.abs

object Day2 extends Exercise(2024, 2) {

  private def prepareListsFromInput(input: List[String]): List[List[Int]] = {
    input.map(_.split(" ").map(_.toInt).toList)
  }

  private def checkUnique(report: List[Int]): Boolean = {
    val set = report.toSet
    report.length == set.size
  }

  private def checkSorted(report: List[Int]): Boolean = {
    val sorted = report.sorted
    val reverseSorted = sorted.reverse

    report == sorted || report == reverseSorted
  }

  @tailrec
  private def checkIncrement(report: List[Int]): Boolean = report match {
    case Nil => true
    case _ :: Nil => true
    case x :: y :: tail =>
      val diff = abs(y - x)
      if (diff >= 1 && diff <= 3) checkIncrement(y :: tail)
      else false
  }

  override def part1(input: List[String]): IO[String] = {
    for {
      prepped <- IO.pure(prepareListsFromInput(input))
    } yield prepped.count(report => checkUnique(report) && checkSorted(report) && checkIncrement(report)).toString
  }

  private def checkWindowed(report: List[Int]): Boolean = {

    def isSafe(window: List[Int]): Boolean = {
      val diff1 = abs(window(1) - window.head)
      val diff2 = abs(window(2) - window(1))
      checkSorted(window) && checkUnique(window) && diff1 >= 1 && diff1 <= 3 && diff2 >= 1 && diff2 <= 3
    }

    def helper(report: List[Int], retry: Boolean): Boolean = {
      val windowSize: Int = 3
      val windowed: List[List[Int]] = report.sliding(windowSize).toList
      val incorrectNums: Option[List[Int]] = windowed.find(!isSafe(_))

      if (incorrectNums.isDefined) {
        if (retry)
          helper(report.patch(report.indexOf(incorrectNums.get.head), Nil, 1), false) ||
          helper(report.patch(report.indexOf(incorrectNums.get(1)), Nil, 1), false) ||
          helper(report.patch(report.indexOf(incorrectNums.get(2)), Nil, 1), false) ||
          helper(report.patch(report.lastIndexOf(incorrectNums.get.head), Nil, 1), false) ||
          helper(report.patch(report.lastIndexOf(incorrectNums.get(1)), Nil, 1), false) ||
          helper(report.patch(report.lastIndexOf(incorrectNums.get(2)), Nil, 1), false)
        else false
      } else {
        true
      }
    }

    helper(report, true)
  }

  override def part2(input: List[String]): IO[String] = {
    for {
      prepped <- IO.pure(prepareListsFromInput(input))
      count = prepped.count(report => checkWindowed(report)).toString
    } yield count
  }
}
