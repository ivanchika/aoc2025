package ic.aoc2025

object Day2 extends Day0 {

    //        override val filename: String = "test_day2.txt"
    override val filename: String = "day2.txt"

    override def partOne(): Unit = {
        val line = readLine(filename)
        val ranges = line.split(",")
        val result = ranges.map(range => {
            val fromTo = range.split("-").map(str => str.toLong).toList
            val from = fromTo.head
            val to = fromTo(1)
            (from to to + 1).map[Long](
                value => {
                    val strVal = value.toString
                    if (strVal.length % 2 == 0) {
                        val (a, b) = strVal.splitAt(value.toString.length / 2)
                        if (a == b) {
                            value
                        } else 0
                    } else {
                        0
                    }
                }
            ).sum
        }).sum
        println(result)


    }

    override def partTwo(): Unit = {
        print(readLine(filename)
            .split(",")
            .map(str => {
                val parts = str.split("-")
                (parts(0).toLong, parts(1).toLong)
            })
            .map { case (from, to) =>
                (from to to).map {
                    value => {
                        val strVal = value.toString
                        val len = strVal.length
                        if ((1 to len / 2).exists(
                            r => {
                                if (len % r == 0) {
                                    val repeated = strVal.take(r) * (len / r)
                                    strVal == repeated
                                }
                                else false
                            })) value else 0L
                    }
                }.sum
            }.sum)
    }
}
