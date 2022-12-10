package dev.sampalmer

import dev.sampalmer.Input.input

import java.nio.file.spi.FileSystemProvider
import java.util.UUID
import scala.annotation.tailrec
import scala.util.matching.Regex

object Day7 {
  case class File(size: Long, name: String)
  val directoryRegex: Regex = "\\$\\scd\\s(.*)".r
  val dirRegex: Regex = "dir\\s(.*)".r
  val fileRegex: Regex = "(\\d+)\\s(.*)".r

  val day7Input: List[String] = input(7)

  @tailrec
  def process(currentDir: String, commands: List[String], folderMap: Map[String, List[File]]): Map[String, List[File]] = {
    if(commands.isEmpty) {
      folderMap
    } else {
      val cmd = commands.head
      val (dir, newMap) = cmd match
        case directoryRegex(name) =>
          val newDirName = if(name == "..") {
            currentDir.split("/").reverse.tail.reverse.mkString("/")
          } else {
            s"$currentDir/$name"
          }
          val updated = folderMap
            .updated(newDirName, folderMap.getOrElse(newDirName, Nil))
          (newDirName, updated)
        case dirRegex(name) =>
          val newDirName = s"$currentDir/$name"
          val updated = folderMap
            .updated(newDirName, folderMap.getOrElse(newDirName, Nil))
          (currentDir, updated)
        case fileRegex(size, name) =>
          val updatedMap = folderMap.updated(currentDir, File(size.toLong, name) :: folderMap.getOrElse(currentDir, Nil))
          (currentDir, updatedMap)
        case _ => (currentDir, folderMap)
      process(dir, commands.tail, newMap)
    }
  }

  def sizes() = {
    val allDirs = process("/", day7Input, Map())
    allDirs.keys.map(key => {
      (key, allDirs.filter(f => f._1.startsWith(key)).values.flatMap(_.map(_.size)).sum)
    }).toMap
  }

  def partOne = {
    sizes().filter(_._2 <= 100000).values.sum
  }

  def partTwo = {
    val sizeMap = sizes()
    val totalSize = sizeMap.find(_._1 == "///").head._2
    val unusedSpace = 70000000 - totalSize
    val requiredSpace = 30000000 - unusedSpace
    sizeMap.filter(_._2 > requiredSpace).values.toList.min
  }
}
