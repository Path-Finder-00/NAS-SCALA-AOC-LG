package com.pg.bigdata.neighborhoodanalytics.aoc.fp

import cats.effect.IO

import scala.collection.immutable

object Day7 extends Exercise(2024, 7) {

  private type PreppedInput = List[(Long, List[Int])] // Cannot be a map, because the test values may be duplicated

  private val input: List[String] = List(
    "190: 10 19",
    "3267: 81 40 27",
    "83: 17 5",
    "156: 15 6",
    "7290: 6 8 6 15",
    "161011: 16 10 13",
    "192: 17 8 14",
    "21037: 9 7 18 13",
    "292: 11 6 16 20 "
  )

  private def prepInput(input: List[String]): PreppedInput = {
    val resultComponents: List[List[String]] = input.map(_.split(":").toList)

    resultComponents.foldLeft(List.empty[(Long, List[Int])]) { case (list, List(key, values)) =>
      val parts: List[Int] = values.strip.split(" ").map(_.toInt).toList
      list :+ (key.toLong, parts)
    }
  }

  private def checkEquation(testValue: Long, calibrationEquation: List[Int]): Boolean = {

    def helper(remainingCalibrationEquation: List[Int], result: Long): Boolean = {
      if (result > testValue) false
      else if (remainingCalibrationEquation.isEmpty) result == testValue
      else {
        val nextValue = remainingCalibrationEquation.head
        val remaining = remainingCalibrationEquation.tail
        helper(remaining, result + nextValue) || helper(remaining, result * nextValue)
      }
    }

    helper(calibrationEquation.tail, calibrationEquation.head)
  }

  override def part1(input: List[String]): IO[String] = {
    for {
      prepped <- IO.pure(prepInput(input))
      sum = prepped.filter((k, v) => checkEquation(k, v)).map(_._1).sum.toString
    } yield sum
  }

  private def concatenate(result: Long, nextValue: Int): Long = {
    (result.toString + nextValue.toString).toLong
  }

  private def checkEquationWithConcatenation(testValue: Long, calibrationEquation: List[Int]): Boolean = {

    def helper(remainingCalibrationEquation: List[Int], result: Long): Boolean = {
      if (result > testValue) false
      else if (remainingCalibrationEquation.isEmpty) result == testValue
      else {
        val nextValue = remainingCalibrationEquation.head
        val remaining = remainingCalibrationEquation.tail
        helper(remaining, result + nextValue) || helper(remaining, result * nextValue) || helper(remaining, concatenate(result, nextValue))
      }
    }

    helper(calibrationEquation.tail, calibrationEquation.head)
  }

    override def part2(input: List[String]): IO[String] = {
        for {
        prepped <- IO.pure(prepInput(input))
        sum = prepped.filter((k, v) => checkEquationWithConcatenation(k, v)).map(_._1).sum.toString
        } yield sum
    }

}
