package ic.aoc2025

object Day9 extends Day0 {

//  override val filename: String = "test_day9.txt"
  override val filename: String = "day9.txt"

  override def partOne(): Unit = {
    val lines = readLines(filename).map(l => l.split(',').map(d => d.toInt)).zipWithIndex.toList
      println(lines.indices.map(
        lp1 => lines.indices.map(
          lp2 => (math.abs(lines(lp1)._1(0) - lines(lp2)._1(0)) + 1L ) * (math.abs(lines(lp1)._1(1) - lines(lp2)._1(1)) + 1L)
        ).max
      ).max)
  }

  override def partTwo(): Unit = {

  }
}
