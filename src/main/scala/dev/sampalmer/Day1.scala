package dev.sampalmer

import scala.util.Using
import java.io.File
import scala.io.Source
import scala.util.{Failure, Success, Using}


object Day1 {
  private def totals: Array[Long] = Using(Source.fromResource("input1")) { source =>
    source.mkString.split("\n\n").map(elf => {
      elf.split("\n").map(_.toLong).sum
    })
  } match
    case Success(value) => value
    case Failure(exception) => throw exception

  def partOne: Long = totals.max
  def partTwo: Long = totals.sorted.reverse.slice(0, 3).sum
}
