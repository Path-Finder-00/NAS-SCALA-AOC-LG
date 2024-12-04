package com.pg.bigdata.neighborhoodanalytics.aoc

import com.pg.bigdata.neighborhoodanalytics.aoc.Common.Result.*
import os.Path

import scala.sys.process.*

private object Common {
  val assetsDir: Path                                  = os.home / ".aoc"
  private def mkUrl(year: Int, day: Int): String       = s"https://adventofcode.com/$year/day/$day"
  private def mkInputUrl(year: Int, day: Int): String  = s"${mkUrl(year, day)}/input"
  private def mkAnswerUrl(year: Int, day: Int): String = s"${mkUrl(year, day)}/answer"

  private def getData(year: Int, day: Int, token: String): String = {
    val curlCommand = Seq("curl", "-b", s"session=$token", mkInputUrl(year, day))
    curlCommand.!!
  }

  def createProblemDir(problemDir: Path): Unit                                                = os.makeDir.all(problemDir)
  def writeInputDataForProblem(year: Int, day: Int, inputDataPath: Path, token: String): Unit = os.write.over(inputDataPath, Common.getData(year, day, token))

  def getTokenFile(dir: os.Path): os.Path = dir / "token"
  def getTokenFrom(tokenFile: os.Path): String =
    scala.sys.env.getOrElse(
      "AOC_SESSION_TOKEN",
      if (os.exists(tokenFile)) os.read(tokenFile).trim()
      else {
        throw new Exception("You must provide your Advent of Code session token as environment variable as AOC_SESSION_TOKEN or in ~/.aoc/token")
      }
    )

  case class First() {
    override def toString: String = "1"
  }
  case class Second() {
    override def toString: String = "2"
  }
  type Part = First | Second

  sealed trait Result {
    val msg: String
  }
  object Result {
    case class Correct(msg: String)   extends Result
    case class Incorrect(msg: String) extends Result
    case class Other(msg: String)     extends Result
  }

  def sendResult[A](year: Int, day: Int, token: String, answer: A, part: Part): Result = {
    val curlCommand =
      Seq(
        "curl",
        "-X",
        "POST",
        "-H",
        "Content-Type: application/x-www-form-urlencoded",
        "-b",
        s"session=$token",
        "-d",
        s"level=$part&answer=${answer.toString}",
        mkAnswerUrl(year, day)
      )

    val response = curlCommand.!!

    val result =
      if (response.contains("not the right answer")) Incorrect("Incorrect - The answer is incorrect!")
      else if (response.contains("the right answer")) Correct("Correct - The answer is correct!")
      else if (response.contains("You don't seem to be solving the right level.  Did you already complete it?"))
        Other("Other - It seems you already solved this exercise, are you sure you are solving the right exercise?")
      else Other(s"Response is: $response")

    result
  }

  final def time[A](prefix: String, block: => A): A = {
    val start  = System.nanoTime()
    val result = block
    val end    = System.nanoTime()
    val (took, unit, color) = (end - start).toDouble / 1000000 match {
      case ms if ms > 1000 => ((ms / 1000).toString, "s", Console.RED)
      case ms if ms > 100  => (ms.toString, "ms", Console.YELLOW)
      case ms              => (ms.toString, "ms", Console.GREEN)
    }
    val timed = s"$prefix time: " + color + s"${"%-3.4s".format(took)}$unit" + Console.RESET
    println(s"$timed - result: $result")
    result
  }
}
