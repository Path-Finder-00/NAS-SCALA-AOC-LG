package com.pg.bigdata.neighborhoodanalytics.aoc.fp

import cats.effect.IO

import scala.annotation.tailrec
import scala.collection.immutable

object Day5 extends Exercise(2024, 5) {

    private type Update = List[Int]
    private type Rules = immutable.Map[Int, List[Int]]

    private val input: List[String] = List(
        "47|53",
        "97|13",
        "97|61",
        "97|47",
        "75|29",
        "61|13",
        "75|53",
        "29|13",
        "97|29",
        "53|29",
        "61|53",
        "97|53",
        "61|29",
        "47|13",
        "75|47",
        "97|75",
        "47|61",
        "75|61",
        "47|29",
        "75|13",
        "53|13",
        "",
        "75,47,61,53,29",
        "97,61,53,29,13",
        "75,29,13",
        "75,97,47,61,53",
        "61,13,29",
        "97,13,75,29,47"
    )

    private def prepRules(input: List[String]): Rules = {
        val rules: List[(Int, Int)] = input.takeWhile(_.nonEmpty).map { rule =>
            val parts: List[Int] = rule.split('|').map(_.toInt).toList
            (parts(0), parts(1))
        }

        rules.foldLeft(immutable.Map.empty[Int, List[Int]]) { case (map, (key, value)) =>
            map.updated(key, map.getOrElse(key, List.empty[Int]) :+ value)
        }
    }

    private def prepUpdates(input: List[String]): List[Update] = {
        input.dropWhile(_.nonEmpty).drop(1).map(_.split(',').map(_.toInt).toList)
    }

    private def adheresToRules(page: Int, rules: Rules, update: Update): Boolean = {
        val pageIndex: Int = update.indexOf(page)
        val pagesAfter: List[Int] = update.drop(pageIndex + 1)
        val rulesForPage: List[Int] = rules.getOrElse(page, List.empty[Int])
        pagesAfter.forall(rulesForPage.contains)
    }

    private def findUpdatesThatAdhereToRules(rules: Rules, updates: List[Update]): List[Update] = {
        updates.filter { update =>
            update.forall(page => adheresToRules(page, rules, update))
        }
    }

    private def middleElement(update: Update): Int = {
        val middleIndex: Int = update.length / 2
        update(middleIndex)
    }

    private def sumMiddleElements(updates: List[Update]): Int = {
        updates.map(middleElement).sum
    }

    override def part1(input: List[String]): IO[String] = {
        for {
            rules <- IO.pure(prepRules(input))
            updates <- IO.pure(prepUpdates(input))
            adheringUpdates = findUpdatesThatAdhereToRules(rules, updates)
            sum = sumMiddleElements(adheringUpdates)
        } yield sum.toString
    }

    private def findUpdatesThatDoNotAdhereToRules(rules: Rules, updates: List[Update]): List[Update] = {
        updates.filterNot { update =>
            update.forall(page => adheresToRules(page, rules, update))
        }
    }

    private def swapElements(update: Update, index1: Int, index2: Int): Update = {
        val element1 = update(index1)
        val element2 = update(index2)
        update.updated(index1, element2).updated(index2, element1)
    }

    private def makeUpdateAdhereToRules(update: Update, rules: Rules): Update = {

        @tailrec
        def helper(helperUpdate: Update, index: Int): Update = {
            if (index >= helperUpdate.length) helperUpdate
            else {
                val page: Int = helperUpdate(index)
                if (!adheresToRules(page, rules, helperUpdate))  {
                    val rulesForPage: List[Int] = rules.getOrElse(page, List.empty[Int])
                    val incorrectElement: Int = helperUpdate.drop(index + 1).find(elem => !rulesForPage.contains(elem)).get
                    val incorrectElementIndex: Int = helperUpdate.indexOf(incorrectElement)
                    helper(swapElements(helperUpdate, index, incorrectElementIndex), 0)
                } else helper(helperUpdate, index + 1)
            }
        }

        helper(update, 0)
    }

    override def part2(input: List[String]): IO[String] = {
        for {
            rules <- IO.pure(prepRules(input))
            updates <- IO.pure(prepUpdates(input))
            notAdheringUpdates = findUpdatesThatDoNotAdhereToRules(rules, updates)
            sum = sumMiddleElements(notAdheringUpdates.map(update => makeUpdateAdhereToRules(update, rules)))
        } yield sum.toString
    }
}
