package com.pg.bigdata.neighborhoodanalytics.aoc.fp

import cats.*
import cats.effect.IO
import cats.implicits.*

import scala.math.abs

object Day1 extends Exercise(2024, 1) {

  case class Pair(locId1: Long, locId2: Long)
  object Pair {
    def apply(locId: String): Pair = {
      val split = locId.split("   ")
      Pair(split(0).toLong, split(1).toLong)
    }
  }

  private def prepInput(input: List[String]): List[Pair] = input.map(locPair => Pair(locPair))

  private def distance(locIdList1: List[Long], locIdList2: List[Long]): Long = (locIdList1 zip locIdList2).map(locIdPair => abs(locIdPair._1 - locIdPair._2)).sum

  override def part1(input: List[String]): IO[String] = {
    for {
      prepped <- IO.pure(prepInput(input))
      list1 = prepped.map(_.locId1).sorted
      list2 = prepped.map(_.locId2).sorted
    } yield distance(list1, list2).toString
  }

  private def groupValues(locIdList: List[Long]): Map[Long, Int] = locIdList.groupBy(identity).view.mapValues(_.size).toMap

  private def similarityScore(locIdList1: List[Long], locIdList2: List[Long]): Long = {
    val counts: Map[Long, Int] = groupValues(locIdList2)
    locIdList1.map(locId => locId * counts.getOrElse(locId, 0)).sum
  }

  override def part2(input: List[String]): IO[String] = {
    for {
      prepped <- IO.pure(prepInput(input))
      list1 = prepped.map(_.locId1)
      list2 = prepped.map(_.locId2)
    } yield similarityScore(list1, list2).toString
  }

}
