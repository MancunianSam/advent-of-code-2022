package dev.sampalmer

import scala.io.Source
import Input._
import scala.util.{Failure, Success, Using}

object Day2 {
  private val day2Input: List[(String, String)] = input(2)
    .map(_.split(" "))
    .map(arr => (arr.head, arr(1)))

  def partOne: Long = {
    val scores = Map("X" -> 1L, "Y" -> 2L, "Z" -> 3L)
    val draw = Map("X" -> "A", "Y" -> "B", "Z" -> "C")
    val win = Map("X" -> "C", "Y" -> "A", "Z" -> "B")

    day2Input.map {
      case (elf, player) if win(player) == elf => scores(player) + 6L
      case (elf, player) if draw(player) == elf => scores(player) + 3L
      case (_, player) => scores(player)
    }.sum
  }

  def partTwo: Long = {
    val scores = Map("A" -> 1L, "B" -> 2L, "C" -> 3L)
    val (rock, paper, scissors) = ("A", "B", "C")
    val elfWin = Map(paper -> rock, scissors -> paper, rock -> scissors)

    day2Input.map {
      case (elf, "X") => scores(elfWin(elf))
      case (elf, "Y") => scores(elf) + 3
      case (elf, "Z") => scores(List(rock, paper, scissors).filter(p => p != elf && p != elfWin(elf)).head) + 6
    }.sum
  }
}
