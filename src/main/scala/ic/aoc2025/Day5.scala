package ic.aoc2025

object Day5 extends Day0 {

  //    override val filename: String = "test_day5.txt"
  override val filename: String = "day5.txt"

  override def partOne(): Unit = {
    val lines = readLines(filename).toList
    val mid = lines.zipWithIndex.filter(p => p._1.isEmpty).map(p => p._2).head

    println((mid + 1 until lines.size).map(
      i => if ((0 until mid).map(j => {
        val spl = lines(j).split("-")
        if (lines(i).toLong >= spl.head.toLong && lines(i).toLong <= spl.tail.head.toLong) 1 else 0
      }).sum > 0) 1 else 0
    ).sum)

  }

  override def partTwo(): Unit = {
    case class Range(start: Long, end: Long)

    val lines = readLines(filename).toList
    val mid = lines.zipWithIndex.filter(p => p._1.isEmpty).map(p => p._2).head
    val sortedEvents = (0 until mid).map(i => Range(lines(i).split("-").head.toLong, lines(i).split("-").tail.head.toLong))
      .flatMap { case Range(s, e) => List((s, +1L), (e, -1L)) }.sortBy(e => (e._1, -e._2))

    var overlapCount = 0L
    var startMerge = 0L
    var mergedList = List.empty[Range]

    sortedEvents.foreach { event =>
      if (overlapCount == 0L && event._2 == 1L) {
        startMerge = event._1
      }

      overlapCount += event._2

      if (overlapCount == 0L && event._2 == -1L) {
        mergedList = mergedList :+ Range(startMerge, event._1)
      }
    }

    println(mergedList.map(item => {math.abs(item._1 - item._2) + 1L}).sum)
  }

}
