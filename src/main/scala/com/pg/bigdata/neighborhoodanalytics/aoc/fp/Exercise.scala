package com.pg.bigdata.neighborhoodanalytics.aoc.fp

import cats.*
import cats.effect.*
import com.pg.bigdata.neighborhoodanalytics.aoc.Common
import com.pg.bigdata.neighborhoodanalytics.aoc.Common.{First, Part, Second}
import org.http4s.*
import org.http4s.ember.server.*
import org.typelevel.log4cats.{Logger, LoggerFactory}
import org.typelevel.log4cats.slf4j.Slf4jFactory
import os.Path

trait Exercise(year: Int, day: Int) extends IOApp {
  private val problemDir: Path     = Common.assetsDir / year.toString / day.toString
  private val inputDataPath: Path  = problemDir / "input.txt"
  private val getToken: IO[String] = IO.pure(Common.getTokenFrom(Common.getTokenFile(Common.assetsDir)))

  private final def prepareResources(): IO[Unit] = {
    for {
      problemDirExists <- IO.pure(os.exists(problemDir))
      _ <-
        if (!problemDirExists) {
          IO.println(s"Directory for problem $year/$day doesn't exist. Creating directory.")
          IO.pure(Common.createProblemDir(problemDir))
          IO.println("Directories created.")
        } else IO.pure(())
      inputDataExists <- IO.pure(os.exists(inputDataPath))
      token           <- getToken
      _ <-
        if (!inputDataExists) {
          IO.println("Data for problem $year/$day doesn't exist. Saving new data.")
          IO.pure(Common.writeInputDataForProblem(year, day, inputDataPath, token))
          IO.println("Data saved successfully.")
        } else IO.pure(())
    } yield ()
  }

  private def part[A](f: => IO[A], part: Part): IO[A] = {
    for {
      result   <- Common.time(s"Part $part", f)
      token    <- getToken
      response <- IO.pure(Common.sendResult(year, day, token, result, part))
      _        <- IO.println(s"Advent of Code response to your answer is: ${response.msg}")
    } yield result
  }

  def part1(input: List[String]): IO[String]
  def part2(input: List[String]): IO[String]

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]

    EmberServerBuilder
      .default[IO]
      .build
      .use { _ =>
        for {
          _               <- prepareResources()
          body            <- IO.pure(os.read.lines(inputDataPath).toList)
          token           <- getToken
          resultPartOne   <- part1(body)
          responsePartOne <- IO.pure(Common.sendResult(year, day, token, resultPartOne, First()))
          _               <- IO.println(s"Advent of Code response to your answer is: ${responsePartOne.msg}")
          resultPartTwo   <- part2(body)
          responsePartTwo <- IO.pure(Common.sendResult(year, day, token, resultPartTwo, Second()))
          _               <- IO.println(s"Advent of Code response to your answer is: ${responsePartTwo.msg}")
        } yield ()
      }
      .as(ExitCode.Success)
  }
}
