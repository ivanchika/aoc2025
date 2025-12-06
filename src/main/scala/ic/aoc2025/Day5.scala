package ic.aoc2025

object Day5 extends Day0 {

  //  override val filename: String = "test_day5.txt"
  override val filename: String = "day5.txt"

  override def partOne(): Unit = {
    val lines = readLines(filename).toList
    val mid = lines.zipWithIndex.filter(p => p._1.isEmpty).map(p => p._2).head

    println((mid + 1 until lines.size).map(
      i => {
        val ingr = lines(i).toLong
        val fresh = (0 until mid).map(j => {
          val spl = lines(j).split("-")
          if (ingr >= spl.head.toLong && ingr <= spl.tail.head.toLong) 1 else 0
        }).sum
        if (fresh > 0) 1 else 0
      }
    ).sum)

  }

  override def partTwo(): Unit = {
  }

}
