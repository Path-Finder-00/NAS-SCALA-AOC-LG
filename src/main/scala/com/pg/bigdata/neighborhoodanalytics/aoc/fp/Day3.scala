package com.pg.bigdata.neighborhoodanalytics.aoc.fp

import cats.effect.IO

import scala.util.matching.Regex

object Day3 extends Exercise(2024, 3) {

  private def findMulMatches(input: List[String]): List[String] = {
    val text: String = input.mkString("")
    val pattern: Regex = "mul\\(\\d{1,3},\\d{1,3}\\)".r
    pattern.findAllIn(text).toList
  }

  private def calculateTotal(matches: List[String]): Int = {
    matches.map { matchStr =>
      val nums = matchStr.drop("mul(".length).dropRight(")".length).split(",").map(_.toInt)
      nums(0) * nums(1)
    }.sum
  }

  override def part1(input: List[String]): IO[String] = {
    for {
      foundMatches <- IO.pure(findMulMatches(input))
      totals = calculateTotal(foundMatches)
    } yield totals.toString
  }

  private def splitByDisabler(input: List[String]): List[String] = {
    val text: String = input.mkString("")
    val pattern: Regex = "don't\\(\\)".r
    pattern.split(text).toList
  }

  private def findEnabledMulMatches(input: List[String]): List[String] = {
    val inputSplitByDisabler: List[String] = splitByDisabler(input)
    val firstFragment: String = inputSplitByDisabler.head // The first fragment always contains only enabled elements
    val inputSplitByDisablerWithoutTheFirstFragment: List[String] = inputSplitByDisabler.tail
    val enabledSubstrings: List[String] = inputSplitByDisablerWithoutTheFirstFragment.flatMap { fragment =>
      val firstDoOccurence: Int = fragment.indexOf("do()")
      if (firstDoOccurence != -1) Some(fragment.substring(firstDoOccurence)) else None
    }
    findMulMatches(firstFragment :: enabledSubstrings)
  }

  override def part2(input: List[String]): IO[String] = {
    for {
      foundMatches <- IO.pure(findEnabledMulMatches(input))
      totals = calculateTotal(foundMatches)
    } yield totals.toString
  }
}
