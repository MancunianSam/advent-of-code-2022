package dev.sampalmer

import dev.sampalmer.Input.input
import scala.language.implicitConversions

object Day4 {
  given stringToRange: Conversion[String, Range] with {
    def apply(s: String): Range = s.split("-") match
      case Array(a, b) => a.toInt to b.toInt
  }

  private def count(fn: (String, String) => Boolean) = input(4).count(_.split(",") match
    case Array(a, b) => fn(a,b) )

  def partOne: Int = count((a, b) => a.containsSlice(b) || b.containsSlice(a))

  def partTwo: Int = count((a, b) => a.intersect(b).nonEmpty)

}
