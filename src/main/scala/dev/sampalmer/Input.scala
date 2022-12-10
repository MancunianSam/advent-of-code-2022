package dev.sampalmer

import scala.io.Source
import scala.util.{Failure, Success, Using}

private object Input {

  def input(num: Int): List[String] = Using(Source.fromResource(s"input$num")) { source =>
    source.mkString.split("\n").toList
  }.getOrElse(Nil)
}
