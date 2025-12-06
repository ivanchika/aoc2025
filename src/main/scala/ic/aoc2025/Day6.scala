package ic.aoc2025

object Day6 extends Day0 {

  //  override val filename: String = "test_day6.txt"
  override val filename: String = "day6.txt"

  override def partOne(): Unit = {
    val input = readLines(filename).toList
    val ops = Seq('+', '*')
    val splitters = input.last.zipWithIndex.filter(l => ops.contains(l._1)).toList
    println(splitters.indices.map(
      i => input.dropRight(1).foldLeft(0L) {
        case (res, line) =>
          val from = splitters(i)._2
          val to = if (i == splitters.length - 1) line.length else splitters(i + 1)._2
          val item = line.slice(from, to).trim.toLong
          splitters(i)._1 match {
            case '+' => res + item
            case '*' => math.max(1, res) * item
          }
      }
    ).sum)
  }

  override def partTwo(): Unit = {
    val input = readLines(filename).toList
    val n = input.map(l => l.length).max
    val rotated = (0 until n).map(i => input.map(line => if (i < line.length) line(i) else "").mkString).toList

    val splitters = rotated.zipWithIndex.filter(l => l._1.trim.isEmpty).map(l => l._2).appended(rotated.length)
    val result = splitters.indices.map(i => {
      val numbers = rotated.slice(if (i == 0) 0 else splitters(i - 1) + 1, splitters(i))
      val op = numbers.head(numbers.head.length - 1)
      numbers.foldLeft(0L) {
        case (res, item) =>
          val num = (if (item.contains(op)) item.dropRight(1) else item).trim.toLong
          op match {
            case '*' => math.max(1, res) * num
            case '+' => res + num
          }
      }
    }).sum
    println(result)
  }
}
