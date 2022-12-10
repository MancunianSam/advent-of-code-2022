package dev.sampalmer

import Input._
import scala.language.implicitConversions

object Day3 {
  private val day3Input = input(3)

  private def priority(c: Char): Long = if (c.isUpper) {
    c.toInt - 38
  } else {
    c.toInt - 96
  }

  def partOne: Long = {
    day3Input.map(rucksack =>
      val arr = rucksack.toCharArray
      val firstHalf = arr.slice(0, arr.length / 2).toSet
      val secondHalf = arr.slice(arr.length / 2, arr.length).toSet
      val commonItem: Char = firstHalf.intersect(secondHalf).head
      priority(commonItem)
    ).sum
  }

  given stringToSet: Conversion[String, Set[Char]] with {
    def apply(s: String): Set[Char] = s.toCharArray.toSet
  }

  def partTwo: Long = day3Input.grouped(3).toList
    .map(group => priority(group.head.intersect(group(1)).intersect(group(2)).head)).sum

}
