package ic.aoc2025

object Day7 extends Day0 {

  //    override val filename: String = "test_day7.txt"
  override val filename: String = "day7.txt"

  override def partOne(): Unit = {
    val lines = readLines(filename).toList
    val start = lines.head.indexOf("S")
    println(
      lines
        .drop(1)
        .foldLeft(0, Set(start)) {
          case (res, line) =>
            if (!line.contains('^'))
              (res._1, res._2)
            else
              (res._1 + res._2.count(i => line(i) == '^'),
                res._2.flatMap(i =>
                  if (line(i) == '^')
                    Seq(i - 1, i + 1)
                  else
                    Seq(i)))
        }._1
    )
  }

  override def partTwo(): Unit = {

  }
}
