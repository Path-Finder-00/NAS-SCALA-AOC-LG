package com.pg.bigdata.neighborhoodanalytics.aoc.fp

import cats.effect.IO

import scala.annotation.tailrec

object Day6 extends Exercise(2024, 6) {

  trait Direction:
    def turn(): Direction

  object Direction:
    case object Up extends Direction:
      def turn(): Direction = Right
    case object Right extends Direction:
      def turn(): Direction = Down
    case object Down extends Direction:
      def turn(): Direction = Left
    case object Left extends Direction:
      def turn(): Direction = Up

  trait Point:
    val x: Int
    val y: Int

  case class Path(x: Int, y: Int) extends Point
  case class Obstacle(x: Int, y: Int) extends Point
  case class Walked(x: Int, y: Int) extends Point
  case class Guard(x: Int, y: Int, direction: Direction) extends Point {
    def turn(): Guard = Guard(x, y, direction.turn())

    def walk(): Guard = direction match {
      case Direction.Up => Guard(x, y - 1, direction)
      case Direction.Right => Guard(x + 1, y, direction)
      case Direction.Down => Guard(x, y + 1, direction)
      case Direction.Left => Guard(x - 1, y, direction)
    }
  }
  case class Map(map: List[Point]) {
    def boundary(): (Int, Int) = {
      val xMax = map.collect { case point: Point => point.x }.max
      val yMax = map.collect { case point: Point => point.y }.max
      (xMax, yMax)
    }

    def guard(): Guard = map.collectFirst { case guard: Guard => guard }.get

    def getPoint(x: Int, y: Int): Point = map.find { case point: Point => point.x == x && point.y == y }.get

    def markWalked(map: List[Point], x: Int, y: Int): List[Point] = {
      map.map {
        case walked: Walked => walked
        case p: Point if p.x == x && p.y == y => Walked(x, y)
        case p: Point => p
      }
    }

    def walk(): List[Point] = {

      @tailrec
      def helper(guard: Option[Guard], map: List[Point]): List[Point] = guard match {
        case None => map
        case Some(guard) =>
          val lookAhead: Guard = guard.walk()
          val x = lookAhead.x
          val y = lookAhead.y
          if (x < 0 || y < 0 || x > boundary()._1 || y > boundary()._2) {
            val newMap: List[Point] = markWalked(map, guard.x, guard.y)
            helper(None, newMap)
          } else if (getPoint(x, y) == Obstacle(x, y)) {
            val newGuard: Option[Guard] = Some(guard.turn().walk())
            val newMap: List[Point] = markWalked(map, guard.x, guard.y)
            helper(newGuard, newMap)
          } else {
            val newGuard: Option[Guard] = Some(lookAhead)
            val newMap: List[Point] = markWalked(map, guard.x, guard.y)
            helper(newGuard, newMap)
          }
      }

      helper(Some(guard()), map)
    }
  }

  private def prepMap(input: List[String]): List[Point] = {
    input.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.map { case (char, x) =>
        char match {
          case '.' => Path(x, y)
          case '#' => Obstacle(x, y)
          case '^' => Guard(x, y, Direction.Up)
          case '>' => Guard(x, y, Direction.Right)
          case 'v' => Guard(x, y, Direction.Down)
          case '<' => Guard(x, y, Direction.Left)
        }
      }
    }
  }

//  val input: List[String] = List(
//    "....#.....",
//    ".........#",
//    "..........",
//    "..#.......",
//    ".......#..",
//    "..........",
//    ".#..^.....",
//    "........#.",
//    "#.........",
//    "......#..."
//  )
//
//  println(prepMap(input))
//  println(Map(prepMap(input)).walk().count(_.isInstanceOf[Walked]))

  override def part1(input: List[String]): IO[String] = {
    for {
      map <- IO.pure(Map(prepMap(input)))
      walked = map.walk()
    } yield walked.count(_.isInstanceOf[Walked]).toString
  }

  override def part2(input: List[String]): IO[String] = ???

}
