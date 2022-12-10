package dev.sampalmer

import scala.annotation.tailrec
import Input._

object Day6 {

  val s: String = input(6).head
  @tailrec
  def process(i: Int, length: Int): Int = {
    if(s.substring(i-length, i).toCharArray.toSet.size == length) {
      i
    } else {
      process(i + 1, length)
    }
  }
  def partOne: Int = process(4, 4)
  def partTwo: Int = process(14, 14)
}
