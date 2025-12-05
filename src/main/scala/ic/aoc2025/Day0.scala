package ic.aoc2025

import scala.io.{BufferedSource, Source}

abstract class Day0 {

   val filename: String
   def main(args: Array[String]): Unit = {
       partOne()
       partTwo()
   }
   def partOne(): Unit
   def partTwo(): Unit

    def readLines(filename: String): Iterator[String] = {
        val source: BufferedSource = Source.fromResource(filename)
        source.getLines()
    }

    def readLine(filename: String): String = {
        val source: BufferedSource = Source.fromResource(filename)
        source.mkString
    }
}
