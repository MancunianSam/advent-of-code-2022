package dev.sampalmer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.UnanchoredRegex
import scala.util.{Failure, Success, Try, Using}

object Day5 {

  private def process(fn: ((Vector[String], Vector[String])) => Vector[String]): String = {

    Using(Source.fromResource(s"input5")) { source =>
      val split = source.mkString.split("\n\n")
      val max = split.head.split("\n").toList.last.split(" ").last.trim.toInt
      val stacks: List[List[String]] = split.head.split("\n").toList.dropRight(1).map(row => {
        "(\\s{3})\\s|(\\[[A-Z]])".r.findAllMatchIn(row).toList.map(_.group(0))
      })

      val vectorStacks = List.fill(max)(Vector[String]()).zipWithIndex.map((_, idx) =>
        val d = stacks.map(a => Try(
          a(idx)).getOrElse("")
          .trim.replace("[", "").replace("]", "")
        )
        d.toVector
      ).map(_.filter(_.nonEmpty))
      case class Instructions(num: Int, from: Int, to: Int)
      val instructionsRegex = "move\\s(\\d+)\\sfrom\\s(\\d)+\\sto\\s(\\d)+".r
      val instructions: Array[Instructions] = split.last.split("\n").map(s => {
        s match
          case instructionsRegex(num, from, to) => Instructions(num.toInt, from.toInt -1, to.toInt -1)
      })
      instructions.foldLeft(vectorStacks)((vli, in) => {
        val from = vli(in.from)
        val to = vli(in.to)
        val toMove = from.reverse.splitAt(from.length - in.num)
        val newVector = to.prependedAll(fn(toMove))
        vli.updated(in.from, toMove._1.reverse).updated(in.to, newVector)
      }).map(_.head).mkString
    } match
      case Failure(exception) => throw exception
      case Success(value) => value
  }

  def partOne: String = process(f => f._2)
  def partTwo: String = process(f => f._2.reverse)
}
