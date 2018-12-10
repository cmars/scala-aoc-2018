import java.io.{BufferedReader, FileInputStream, FileReader}

import scala.collection.mutable
import scala.io.Source

object Day1JavaInScala {
  def main(args: Array[String]): Unit = {
    val br = new BufferedReader(new FileReader("day/1/input"))
    var line = br.readLine()
    var freq = 0
    while (line != null) {
      val offset = Integer.parseInt(line)
      freq += offset
      line = br.readLine()
    }
    println(freq)
  }
}

// scala native
object Day1 {
  def main(args: Array[String]): Unit = {
    val freq = Source.fromFile("day/1/input").getLines().map(Integer.parseInt).foldLeft(0) {(acc, offset) => acc + offset}
    println(freq)
  }
}

object Day1Part2 {
  def main(args: Array[String]): Unit = {
    val offsets: List[Int] = Source.fromFile("day/1/input").getLines().map(Integer.parseInt).toList
    // implement an endless iterator over offsets
    val repeatingOffsets = new OffsetIterator(offsets = offsets)
    val seen = new mutable.HashSet[Integer]
    def result(f: Int): Int = {
      if (!seen.add(f)) {
        f
      } else {
        val nextF = f + repeatingOffsets.next()
        result(nextF)
      }
    }
    println(result(0))
  }
}

class OffsetIterator(offsets: List[Int]) extends Iterator[Int] {
  var i: Iterator[Int] = offsets.iterator

  def hasNext = offsets.nonEmpty

  def next() = {
    if (i.hasNext) i.next() else {
      i = offsets.iterator
      i.next()
    }
  }
}