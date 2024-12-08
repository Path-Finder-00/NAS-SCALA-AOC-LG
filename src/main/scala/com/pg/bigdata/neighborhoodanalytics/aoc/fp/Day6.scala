package com.pg.bigdata.neighborhoodanalytics.aoc.fp

import cats.effect.{IO, Sync}

import scala.annotation.tailrec
import scala.math.abs

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
  case class NewObstacle(x: Int, y: Int) extends Point
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

    def overBoundary(point: Point): Boolean = point.x < 0 || point.y < 0 || point.x > boundary()._1 || point.y > boundary()._2

    def guard(): Guard = map.collectFirst { case guard: Guard => guard }.get

    def obstacleInBetween(x1: Int, y1: Int, x2: Int, y2: Int, direction: Direction): Option[Point] = {
      direction match {
        case Direction.Up => if (abs(y2 - y1) <= 1) None else map.collectFirst { case obstacle: Obstacle if obstacle.x == x1 && obstacle.y > y1 && obstacle.y < y2 => obstacle }
        case Direction.Right => if (abs(x2 - x1) <=1) None else map.collectFirst { case obstacle: Obstacle if obstacle.x > x1 && obstacle.x < x2 && obstacle.y == y1 => obstacle }
        case Direction.Down => if (abs(y2 - y1) <= 1) None else map.collectFirst { case obstacle: Obstacle if obstacle.x == x1 && obstacle.y > y1 && obstacle.y < y2 => obstacle }
        case Direction.Left => if (abs(x2 - x1) <= 1) None else map.collectFirst { case obstacle: Obstacle if obstacle.x > x1 && obstacle.x < x2 && obstacle.y == y1 => obstacle }
      }
    }

    def getPoint(x: Int, y: Int): Point = map.find { case point: Point => point.x == x && point.y == y }.get

    def markWalked(map: List[Point], guard: Guard): List[Point] = {
      val x = guard.x
      val y = guard.y
      map.map {
        case p: Point if p.x == x && p.y == y => Walked(x, y)
        case p: Point => p
      }
    }

    def markNewObstacle(map: List[Point], initialDirection: Direction, guard: Guard, obstacle: Obstacle): List[Point] = {
      val newObstacle: NewObstacle = initialDirection match {
        case Direction.Up =>
          NewObstacle(obstacle.x - 1, guard.y)
        case Direction.Right =>
          NewObstacle(guard.x, obstacle.y - 1)
        case Direction.Down =>
          NewObstacle(obstacle.x + 1, guard.y)
        case Direction.Left =>
          NewObstacle(guard.x, obstacle.y + 1)
      }
      map.map {
        case p: Point if p.x == newObstacle.x && p.y == newObstacle.y => newObstacle
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
          if (overBoundary(lookAhead)) {
            val newMap: List[Point] = markWalked(map, guard)
            helper(None, newMap)
          } else if (getPoint(x, y) == Obstacle(x, y)) {
            val newGuard: Option[Guard] = Some(guard.turn().walk())
            val newMap: List[Point] = markWalked(map, guard)
            helper(newGuard, newMap)
          } else {
            val newGuard: Option[Guard] = Some(lookAhead)
            val newMap: List[Point] = markWalked(map, guard)
            helper(newGuard, newMap)
          }
      }

      helper(Some(guard()), map)
    }

    def placeNewObstacles(): List[Point] = {

      def walkUntilNewObstacleIsPlaced(map: List[Point], initialDirection: Direction, guard: Guard, obstacle: Obstacle): List[Point] = {

        @tailrec
        def helper(helperGuard: Option[Guard], helperMap: List[Point]): List[Point] = {

          if (helperGuard.isEmpty) {
            helperMap
          } else {
            val lookAhead: Guard = helperGuard.get.walk()
            val x = lookAhead.x
            val y = lookAhead.y
            if (overBoundary(lookAhead)) {
              helper(None, helperMap)
            } else if (getPoint(x, y) == Obstacle(x, y)) {
              val newGuard: Option[Guard] = Some(helperGuard.get.turn().walk())
              helper(newGuard, helperMap)
            } else {
              val newMap: List[Point] = if (canPlaceNewObstacle(Map(helperMap), initialDirection, helperGuard.get, obstacle)) {
                val temp = Map(helperMap).markNewObstacle(helperMap, initialDirection, helperGuard.get, obstacle)
                println(temp.count(_.isInstanceOf[NewObstacle]))
                temp
              } else {
                helperMap
              }
              val newGuard: Option[Guard] = Some(lookAhead)
              helper(newGuard, newMap)
            }
          }
        }

        helper(Some(guard), map)
      }

      @tailrec
      def helper(guard: Option[Guard], map: List[Point]): List[Point] = guard match {
        case None =>
          map
        case Some(guard) =>
          val lookAhead: Guard = guard.walk()
          val x = lookAhead.x
          val y = lookAhead.y
          if (overBoundary(lookAhead)) {
            helper(None, map)
          } else if (getPoint(x, y) == Obstacle(x, y)) {
            val newMap: List[Point] = walkUntilNewObstacleIsPlaced(map, guard.direction, guard, Obstacle(x, y))
            val newGuard: Option[Guard] = Some(guard.turn().walk())
            helper(newGuard, newMap)
          } else {
            val newGuard: Option[Guard] = Some(lookAhead)
            helper(newGuard, map)
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

  val input: List[String] = List(
    "....#.....",
    ".........#",
    "..........",
    "..#.......",
    ".......#..",
    "..........",
    ".#..^.....",
    "........#.",
    "#.........",
    "......#..."
  )
//
//  println(prepMap(input))
//  println(Map(prepMap(input)).walk().count(_.isInstanceOf[Walked]))

  override def part1(input: List[String]): IO[String] = {
    for {
      map <- IO.pure(Map(prepMap(input)))
      walked = map.walk()
    } yield walked.count(_.isInstanceOf[Walked]).toString
  }

  private def canPlaceNewObstacle(map: Map, initialDirection: Direction, guard: Guard, obstacle: Obstacle): Boolean = {
    initialDirection match {
      case Direction.Up =>
        guard.y > obstacle.y && guard.x >= obstacle.x && guard.direction == Direction.Left && map.obstacleInBetween(obstacle.x - 1, guard.y, guard.x, guard.y, Direction.Left).isEmpty
      case Direction.Right =>
        guard.x < obstacle.x && guard.y >= obstacle.y && guard.direction == Direction.Up && map.obstacleInBetween(guard.x, obstacle.y - 1, guard.x, guard.y, Direction.Up).isEmpty
      case Direction.Down =>
        guard.y < obstacle.y && guard.x <= obstacle.x && guard.direction == Direction.Right && map.obstacleInBetween(guard.x, guard.y, obstacle.x + 1, guard.y, Direction.Right).isEmpty
      case Direction.Left =>
        guard.x > obstacle.x && guard.y <= obstacle.y && guard.direction == Direction.Down && map.obstacleInBetween(guard.x, guard.y, guard.x, obstacle.y + 1, Direction.Down).isEmpty
    }
  }

  println(Map(prepMap(input)).placeNewObstacles().count(_.isInstanceOf[NewObstacle]))

  override def part2(input: List[String]): IO[String] = {
    for {
      map <- IO.pure(Map(prepMap(input)))
      newObstacles = map.placeNewObstacles()
      count = newObstacles.count(_.isInstanceOf[NewObstacle]).toString
      _ = println(count)
    } yield count
  }

}
