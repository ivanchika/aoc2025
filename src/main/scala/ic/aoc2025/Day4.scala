package ic.aoc2025

import scala.collection.mutable

object Day4 extends Day0 {

//    override val filename: String = "test_day4.txt"
  override val filename: String = "day4.txt"

  private val moves = Seq(
    (-1, -1), (-1, 0), (-1, 1),
    (0, -1), (0, 1),
    (1, -1), (1, 0), (1, 1)
  )

  override def partOne(): Unit = {
    val array = readLines(filename)
      .map(line => line.toCharArray)
      .toArray
    val n = array.length
    val m = array(0).length

    println(array.indices.map(i => {
      array(i).indices.map(j => {
        if ((array(i)(j) == '@')
          && moves.map(move => {
          val y = i + move._1
          val x = j + move._2
          if (x >= 0 && y >= 0 && y < n && x < m && array(y)(x) == '@') 1 else 0
        }).sum < 4) 1 else 0
      }).sum
    }).sum)
  }

  override def partTwo(): Unit = {
    val array = readLines(filename)
      .map(line => line.toCharArray)
      .toArray
    val n = array.length
    val m = array(0).length

    val removed = mutable.Set[(Int, Int)]()
    var count = 0

    while (array.indices.map(i => {
        array(i).indices.map(j => {
          if ((array(i)(j) == '@')
            && moves.map(move => {
            val y = i + move._1
            val x = j + move._2
            if (x >= 0 && y >= 0 && y < n && x < m && array(y)(x) == '@') 1 else 0
          }).sum < 4) {
            removed.add((i, j))
            1
          } else 0
        }).sum
      }).sum > 0
    ) {
      removed.foreach(pair => {
        array(pair._1)(pair._2) = '.'
      })
    }

    println(removed.size)
  }

}
