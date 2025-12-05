package ic.aoc2025

object Day1 extends Day0 {

    val filename = "day1.txt"
//    val filename = "test_day1.txt"
    val START: Int = 50
    val DIAL_SIZE: Int = 100

    def partOne(): Unit = {
        println("Part One")
        val lines = readLines(filename)
        var pos: Int = START
        var count = 0
        for (line <- lines) {
            val steps = line.tail.toInt
            val rotation = line.head match {
                case 'L' => -steps
                case _ => steps
            }
            pos = (pos + (rotation % DIAL_SIZE) + DIAL_SIZE) % DIAL_SIZE
            if (pos == 0) count = count + 1
        }
        println("Result: " + count)
    }

    def partTwo(): Unit = {
        println("Part Two")
        val lines = readLines(filename)
        var pos: Int = START
        var count: Int = 0
        for (line <- lines) {
            val steps = line.tail.toInt
            val rotation = line.head match {
                case 'L' => -steps
                case 'R' => steps
                case _ => 0
            }
            val zeros: Int = line.head match {
                case 'L' => if (steps < pos) 0 else if (pos == 0) steps / DIAL_SIZE else (math.abs(pos - steps) / DIAL_SIZE) + 1
                case 'R' => (pos + steps) / DIAL_SIZE
                case _ => 0
            }
            pos = (pos + (rotation % DIAL_SIZE) + DIAL_SIZE) % DIAL_SIZE
            count = count + zeros
        }
        println("Result: " + count)
    }

}
