package dev.sampalmer

import Input.input

import scala.annotation.tailrec

object Day10 {
  private val day10Input: List[String] = input(10)
  private val instructions: List[CurrentInstruction] = day10Input.map(i => {
    if (i == "noop") {
      CurrentInstruction(1, l => l)
    } else {
      CurrentInstruction(2, l => i.split(" ").last.toLong + l)
    }
  })

  private case class CurrentInstruction(cyclesLeft: Int, valueWhenComplete: Long => Long)

  private case class Answers(idx: Int, answer: Long)

  @tailrec
  private def process(currentCycle: Int, instructions: List[CurrentInstruction], nextX: Long, answers: List[Answers]): List[Answers] = {
    if (instructions.isEmpty) {
      answers
    } else {
      val decrementedCycle = instructions.head.copy(cyclesLeft = instructions.head.cyclesLeft - 1)
      val (newX, newInstructions) = if (decrementedCycle.cyclesLeft == 0) {
        (instructions.head.valueWhenComplete(nextX), instructions.tail)
      } else {
        (nextX, decrementedCycle :: instructions.tail)
      }
      process(currentCycle + 1, newInstructions, newX, Answers(currentCycle, nextX) :: answers)
    }
  }

  def partOne =
    process(1, instructions, 1, Nil)
      .filter(a => List(20, 60, 100, 140, 180, 220).contains(a.idx)).map(r => r.answer * r.idx).sum

  def partTwo = {
    val answers = process(1, instructions, 1, Nil).map(p => p.idx -> p.answer).toMap
    val output = (1 to 6).toList.map(_ => List.fill(40)("."))
    val totalSize = output.flatten.size
    val outputArray = (0 until totalSize).toList.foldLeft(output)((li, r) => {
      val spritePos = List(answers(r + 1) -1, answers(r + 1), answers(r + 1) + 1)
      if(spritePos.contains(r % 40)) {
        li.updated(r/40, li(r/40).updated(r % 40, "#"))
      } else {
        li
      }
    })
    outputArray.map(o => o.mkString).mkString("\n")
  }
}
