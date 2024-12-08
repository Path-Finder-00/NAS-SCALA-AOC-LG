package com.pg.bigdata.neighborhoodanalytics.aoc.fp

import cats.effect.IO

import scala.collection.immutable

object Day8 extends Exercise(2024, 8) {

  val input: List[String] = List(
    "............",
    "........0...",
    ".....0......",
    ".......0....",
    "....0.......",
    "......A.....",
    "............",
    "............",
    "........A...",
    ".........A..",
    "............",
    "............"
  )

  case class Point(x: Int, y: Int)

  case class Map(map: immutable.Map[Char, List[Point]]) {
    def boundary(): (Int, Int) = {
      val xMax = map.values.flatten.map(_.x).max
      val yMax = map.values.flatten.map(_.y).max
      (xMax, yMax)
    }

    def overBoundary(point: Point): Boolean = {
      val (xMax, yMax) = boundary()
      point.x > xMax || point.y > yMax || point.x < 0 || point.y < 0
    }

    def makeCombinations(char: Char): List[(Point, Point)] = {
      val points: List[Point] = map(char)
      val temp = points.combinations(2).toList.map {
        case List(a, b) => (a, b)
        case List(_) => (Point(0, 0), Point(0, 0))
      }
      println(temp)
      temp
    }

    def findAntinode(combination: (Point, Point)): Option[Point] = {
//      val slope: Double = (combination._2.y - combination._1.y) / (combination._2.x - combination._1.x)
//      val intercept: Double = combination._1.y - slope * combination._1.x
//      def line(x: Int): Int = (slope * x + intercept).toInt
//      val vectorP1P2: Point = Point(combination._2.x - combination._1.x, combination._2.y - combination._1.y)
      val p3x: Int = 2 * combination._2.x - combination._1.x
      val p3y: Int = 2 * combination._2.y - combination._1.y
      val p3: Point = Point(p3x, p3y)
      if (!overBoundary(p3)) Some(p3) else None
    }

    def findAntinodesPerCombination(combination: (Point, Point)): Set[Point] = {
      if (combination._1 == combination._2) Set.empty[Point]
      else {
        List(combination, combination.swap).foldLeft(Set.empty[Point]) { case (set, combination) =>
          findAntinode(combination) match {
            case Some(point) => set + point
            case None => set
          }
        }
      }
    }

    def findAntinodes(char: Char): Set[Point] = {
      makeCombinations(char).foldLeft(Set.empty[Point]) { case (set, combination) =>
        set ++ findAntinodesPerCombination(combination)
      }
    }

    def findAntinodesForAllCharacters: Set[Point] = {
      map.keys.filter(_ != '.').foldLeft(Set.empty[Point]) { case (set, char) =>
        set ++ findAntinodes(char)
      }
    }

    def countAllAntinodes: Int = {
      findAntinodesForAllCharacters.size
    }
  }

  object Map {
    def apply(input: List[String]): Map = {
      val chars: List[(Char, Point)] = input.zipWithIndex.flatMap { case (line, y) =>
        line.zipWithIndex.map { case (char, x) =>
          (char, Point(x, y))
        }
      }

      Map(chars.foldLeft(immutable.Map.empty[Char, List[Point]]) { case (map, (key, value)) =>
        map.updated(key, map.getOrElse(key, List.empty[Point]) :+ value)
      })
    }
  }

  println(Map(input))
  println(Map(input).countAllAntinodes)

  override def part1(input: List[String]): IO[String] = {
    IO.pure(Map(input).countAllAntinodes.toString)
  }

  override def part2(input: List[String]): IO[String] = ???
}
