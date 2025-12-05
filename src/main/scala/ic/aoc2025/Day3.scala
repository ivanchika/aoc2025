package ic.aoc2025

object Day3 extends Day0 {


  override val filename: String = "day3.txt"
  //  override val filename: String = "test_day3.txt"

  override def partOne(): Unit = {
    println(readLines(filename)
      .map(line => {
        val first = line.slice(0, line.length - 1).max
        val second = line.slice(line.indexOf(first) + 1, line.length).max
        (first + "" + second).toInt
      }).sum)
  }

  override def partTwo(): Unit = {
    println(readLines(filename)
      .map(line => {
        val indexed = line.zipWithIndex
        (1 to 12).foldLeft(("", -1)) {
          case ((res, lastIndex), step) =>
            val max = indexed.slice(lastIndex + 1, indexed.size - (12 - step)).maxBy(pair => pair._1)
            (res + max._1, max._2)
        }
      })
      .map((a, b) => a.mkString.toLong)
      .sum
    )
  }
}
