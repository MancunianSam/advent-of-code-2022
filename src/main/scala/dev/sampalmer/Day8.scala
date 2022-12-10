package dev.sampalmer

import Input.input

object Day8 {
  private val day8Input: List[String] = input(8)
  private val trees: List[List[Int]] = day8Input.map(_.toCharArray.toList.map(_.toString.toInt))
  private val width: Int = trees.length - 1
  private val height: Int = trees.head.length - 1

  def partOne: Int = {
    trees.zipWithIndex.flatMap { case (treeX, y) =>
      treeX.zipWithIndex.map { case (treeY, x) =>
        partOneFn(y, treeY, x)
      }
    }.count(b => b)
  }

  def partTwo: Int = {
    trees.zipWithIndex.flatMap { case (treeX, y) =>
      treeX.zipWithIndex.map { case (currentTree, x) =>
        partTwoFn(y, currentTree, x)
      }
    }.max
  }

  extension (i: Int)
    private def toCount(y: Int): Int = i match
      case -1 => y
      case v: Int => v + 1

  private def partTwoFn(y: Int, currentTree: Int, x: Int) = {
    val (rangeAbove, rangeBelow, rangeLeft, rangeRight) = getRanges(y, x)
    val countAbove = rangeAbove.map(r => trees(r)(x)).reverse.indexWhere(v => v >= currentTree).toCount(y)
    val countBelow = rangeBelow.map(r => trees(r)(x)).indexWhere(v => v >= currentTree).toCount(height - y)
    val countLeft = rangeLeft.map(r => trees(y)(r)).reverse.indexWhere(v => v >= currentTree).toCount(x)
    val countRight = rangeRight.map(r => trees(y)(r)).indexWhere(v => v >= currentTree).toCount(width - x)
    val sum = countAbove * countBelow * countLeft * countRight
    sum
  }

  private def partOneFn(y: Int, currentTree: Int, x: Int) = {
    val isOuter = x == 0 || y == 0 || y == width || x == height
    if (isOuter) {
      true
    } else {
      val (rangeAbove, rangeBelow, rangeLeft, rangeRight) = getRanges(y, x)

      val blockedAbove = rangeAbove.exists(r => trees(r)(x) >= currentTree)
      val blockedBelow = rangeBelow.exists(r => trees(r)(x) >= currentTree)
      val blockedLeft = rangeLeft.exists(r => trees(y)(r) >= currentTree)
      val blockedRight = rangeRight.exists(r => trees(y)(r) >= currentTree)
      !blockedAbove || !blockedBelow || !blockedLeft || !blockedRight
    }
  }

  private def getRanges(y: Int, x: Int) = {
    val rangeAbove: List[Int] = (0 until y).toList
    val rangeBelow: List[Int] = (y + 1 to height).toList
    val rangeLeft: List[Int] = (0 until x).toList
    val rangeRight: List[Int] = (x + 1 to width).toList
    (rangeAbove, rangeBelow, rangeLeft, rangeRight)
  }
}
