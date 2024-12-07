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

    def obstacle(x: Int, y: Int, direction: Direction): Option[Point] = {
      direction match {
        case Direction.Up => map.collectFirst { case obstacle: Obstacle if obstacle.x == x && obstacle.y < y => obstacle }
        case Direction.Right => map.collectFirst { case obstacle: Obstacle if obstacle.x > x && obstacle.y == y => obstacle }
        case Direction.Down => map.collectFirst { case obstacle: Obstacle if obstacle.x == x && obstacle.y > y => obstacle }
        case Direction.Left => map.collectFirst { case obstacle: Obstacle if obstacle.x < x && obstacle.y == y => obstacle }
      }
    }

    def obstacleInBetween(x1: Int, y1: Int, x2: Int, y2: Int, direction: Direction): Option[Point] = {
        direction match {
          case Direction.Up => map.collectFirst { case obstacle: Obstacle if obstacle.x == x1 && obstacle.y > y1 && obstacle.y < y2 => obstacle }
          case Direction.Right => map.collectFirst { case obstacle: Obstacle if obstacle.x > x1 && obstacle.x < x2 && obstacle.y == y1 => obstacle }
          case Direction.Down => map.collectFirst { case obstacle: Obstacle if obstacle.x == x1 && obstacle.y > y1 && obstacle.y < y2 => obstacle }
          case Direction.Left => map.collectFirst { case obstacle: Obstacle if obstacle.x > x1 && obstacle.x < x2 && obstacle.y == y1 => obstacle }
        }
    }

    def getPoint(x: Int, y: Int): Point = map.find { case point: Point => point.x == x && point.y == y }.get

    def markWalked(map: List[Point], guard: Guard): List[Point] = {
      val x = guard.x
      val y = guard.y
      map.map {
        case walked: Walked => walked
        case p: Point if p.x == x && p.y == y => Walked(x, y)
        case p: Point => p
      }
    }

    def markNewObstacle(map: List[Point], obstacle: Obstacle): List[Point] = {
      map.map {
        case p: Point if p.x == obstacle.x && p.y == obstacle.y => NewObstacle(obstacle.x, obstacle.y)
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

    def findNewObstacles(): List[Point] = {

      @tailrec
      def helper(guard: Option[Guard], map: List[Point]): List[Point] = guard match {
        case None => map
        case Some(guard) =>
          val lookAhead: Guard = guard.walk()
          val x = lookAhead.x
          val y = lookAhead.y
          if (overBoundary(lookAhead)) {
            helper(None, map)
          } else if (getPoint(x, y) == Obstacle(x, y)) {
            val newGuard: Option[Guard] = Some(guard.turn().walk())
            val newObstacle: Option[Obstacle] = loopSchema(guard.direction, Obstacle(x, y), Map(map))
            newObstacle match {
              case Some(newObstacle) => helper(newGuard, markNewObstacle(map, newObstacle))
              case None => helper(newGuard, map)
            }
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

//  println(prepMap(input))
//  println(Map(prepMap(input)).walk().count(_.isInstanceOf[Walked]))

  override def part1(input: List[String]): IO[String] = {
    for {
      map <- IO.pure(Map(prepMap(input)))
      walked = map.walk()
    } yield walked.count(_.isInstanceOf[Walked]).toString
  }

  // Mapa koordynatów, gdzie (x, y) to lokalizacja przeszkody:
  // góra: (x, y) (z, y - 1) (z - 1, w) stawiam przeszkodę na (x - 1, w - 1) gdzie z > x, w > y
  // prawo: (x, y) (x - 1, w) (z, w - 1) stawiam przeszkodę na (z - 1, y + 1) gdzie z > x, w < y
  // dół: (x, y) (z, y + 1) (z + 1, w) stawiam przeszkodę na (x + 1, w - 1) gdzie z < x, w > y
  // lewo (x, y) (x + 1, w) (z, w - 1) stawiam przeszkodę na (z - 1, y + 1) gdzie z > x, w > y

  // Po każdym skręcie  patrzę na lokalizację kolejnej przeszkody -
  // zależnie od kierunku strażnika obliczam lokalizację mojej przeszkody na podstawie powyższej mapy.
  // Jeśli lokalizacja którejkolwiek przeszkody jest poza mapą - kończę.

  // Direction is the direction in which the guard is facing at the beginning of the loop, the obstacle is the first one he is facing
  private def loopSchema(direction: Direction, obstacle: Obstacle, map: Map): Option[Obstacle] = {

    def checkNoneEmpty(secondObstacle: Option[Point], thirdObstacle: Option[Point], newObstacle: Option[Obstacle]): Option[Obstacle] =
      if (secondObstacle.isEmpty || thirdObstacle.isEmpty || map.overBoundary(newObstacle.getOrElse(Obstacle(-1, -1)))) None else newObstacle

    direction match {
      case Direction.Up =>
        val secondObstacle: Option[Point] = map.obstacle(obstacle.x, obstacle.y + 1, Direction.Right)
        val thirdObstacle: Option[Point] = secondObstacle match {
          case Some(secondObstacle) => map.obstacle(secondObstacle.x - 1, secondObstacle.y, Direction.Down)
          case None => None
        }
        val newObstacle: Option[Obstacle] = thirdObstacle match {
          case Some(thirdObstacle) => map.obstacleInBetween(obstacle.x, thirdObstacle.y - 1, thirdObstacle.x, thirdObstacle.y - 1, Direction.Left) match {
            case Some(_) => None
            case None => Option(Obstacle(obstacle.x - 1, thirdObstacle.y - 1)) // If there is an obstacle in the way, return None // Option(Obstacle(obstacle.x - 1, thirdObstacle.y - 1))
          }
          case None => None
        }
        println(newObstacle)
        checkNoneEmpty(secondObstacle, thirdObstacle, newObstacle)
      case Direction.Right =>
        val secondObstacle: Option[Point] = map.obstacle(obstacle.x - 1, obstacle.y, Direction.Down)
        val thirdObstacle: Option[Point] = secondObstacle match {
          case Some(secondObstacle) => map.obstacle(secondObstacle.x, secondObstacle.y - 1, Direction.Left)
          case None => None
        }
        val newObstacle: Option[Obstacle] = thirdObstacle match {
          case Some(thirdObstacle) => map.obstacleInBetween(thirdObstacle.x + 1, obstacle.y, thirdObstacle.x + 1, thirdObstacle.y, Direction.Up) match {
            case Some(_) => None
            case None => Option(Obstacle(thirdObstacle.x + 1, obstacle.y - 1)) // If there is an obstacle in the way, return None // Option(Obstacle(thirdObstacle.x + 1, obstacle.y - 1))
          }
          case None => None
        }
        println(newObstacle)
        checkNoneEmpty(secondObstacle, thirdObstacle, newObstacle)
      case Direction.Down =>
        val secondObstacle: Option[Point] = map.obstacle(obstacle.x, obstacle.y - 1, Direction.Left)
        val thirdObstacle: Option[Point] = secondObstacle match {
          case Some(secondObstacle) => map.obstacle(secondObstacle.x + 1, secondObstacle.y, Direction.Up)
          case None => None
        }
        val newObstacle: Option[Obstacle] = thirdObstacle match {
          case Some(thirdObstacle) => map.obstacleInBetween(thirdObstacle.x, thirdObstacle.y + 1, obstacle.x, thirdObstacle.y + 1, Direction.Right) match {
            case Some(_) => None
            case None => Option(Obstacle(obstacle.x + 1, thirdObstacle.y + 1)) // If there is an obstacle in the way, return None
          }
          case None => None
        }
        println(newObstacle)
        checkNoneEmpty(secondObstacle, thirdObstacle, newObstacle)
      case Direction.Left =>
        val secondObstacle: Option[Point] = map.obstacle(obstacle.x + 1, obstacle.y, Direction.Up)
        val thirdObstacle: Option[Point] = secondObstacle match {
          case Some(secondObstacle) => map.obstacle(secondObstacle.x, secondObstacle.y + 1, Direction.Right)
          case None => None
        }
        val newObstacle: Option[Obstacle] = thirdObstacle match {
          case Some(thirdObstacle) => map.obstacleInBetween(thirdObstacle.x - 1, thirdObstacle.y, thirdObstacle.x - 1, obstacle.y, Direction.Down) match {
            case Some(_) => None
            case None => Option(Obstacle(thirdObstacle.x - 1, obstacle.y + 1)) // If there is an obstacle in the way, return None
          }
          case None => None
        }
        println(newObstacle)
        checkNoneEmpty(secondObstacle, thirdObstacle, newObstacle)
    }
  }

  println(Map(prepMap(input)).findNewObstacles().count(_.isInstanceOf[NewObstacle]))

  override def part2(input: List[String]): IO[String] = ???
//  {
//    for {
//      map <- IO.pure(Map(prepMap(input)))
//      newObstacles = map.findNewObstacles()
//    } yield newObstacles.count(_.isInstanceOf[NewObstacle]).toString
//  }

}
