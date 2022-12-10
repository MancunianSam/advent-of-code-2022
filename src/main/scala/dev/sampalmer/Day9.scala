package dev.sampalmer

import Input.input
import scala.annotation.tailrec

object Day9 {
  private val day9Input: List[String] = input(9)

  case class Head(x: Int, y: Int)

  case class Tail(x: Int, y: Int)

  @tailrec
  private def process(input: List[String], positions: Seq[Tail], currentHead: Head, currentTails: Seq[Tail]): Seq[Tail] = {
    if (input.isEmpty) {
      positions
    } else {
      val instruction = input.head
      val (direction, length) = instruction.split(" ") match
        case Array(a, b) => (a, b)
      val range = (1 to length.toInt).toList
      val newPositions: (Head, Seq[Tail], Seq[Tail]) = range.foldLeft((currentHead, currentTails, Seq[Tail]()))((a, r) => {
        val (_, newCurrentTails, newPositions) = a
        val newHead = getNewPosition(currentHead, direction, r)
        val tails1 = currentTails.indices.foldLeft(Seq[Tail]())((cTails, idx) => {
          val toFollow = cTails.lastOption.getOrElse(Tail(newHead.x, newHead.y))
          cTails :+ follow(toFollow, newCurrentTails(idx))
        })
        (newHead, tails1, newPositions :+ tails1.last)
      })
      val nextHead = newPositions._1
      val nextTails = newPositions._2
      val allPositions = newPositions._3
      process(input.tail, positions ++ allPositions, nextHead, nextTails)
    }
  }

  private def getInt(diff: Int): Int = {
    val divided = diff.toDouble/2.0
    if(diff < 0) {
      Math.floor(divided).toInt
    } else {
      Math.ceil(divided).toInt
    }
  }

  private def follow(currentHead: Tail, currentTail: Tail): Tail = {
    val xDiff = currentHead.x - currentTail.x
    val yDiff = currentHead.y - currentTail.y
    if(xDiff.abs > 1) {
      Tail(currentTail.x + (xDiff/2), currentTail.y + getInt(yDiff))
    } else if(yDiff.abs > 1) {
      Tail(currentTail.x + getInt(xDiff), currentTail.y + (yDiff/2))
    } else {
      currentTail
    }
  }

  private def getNewPosition(head: Day9.Head, direction: String, r: Int) = {
    direction match
      case "R" => Head(head.x + r, head.y)
      case "L" => Head(head.x - r, head.y)
      case "U" => Head(head.x, head.y + r)
      case "D" => Head(head.x, head.y - r)
  }

  def partOne: Int = {
    process(day9Input, Nil, Head(0, 0), Seq(Tail(0, 0))).toSet.size
  }
  def partTwo: Int = {
    process(day9Input, Nil, Head(0, 0), Seq.fill(9)(Tail(0, 0))).toSet.size
  }
}
