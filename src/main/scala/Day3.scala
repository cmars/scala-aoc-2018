import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

class ClaimParser extends RegexParsers {
  def id: Parser[String] = """#\d+""".r ^^ {_.toString}
  def number: Parser[Int] = """(\d+)""".r ^^ {_.toInt}
  def claim: Parser[Claim] = id ~ """@""" ~ number ~ "," ~ number ~ ":" ~ number ~ "x" ~ number ^^ {
    case id ~ _ ~ x ~ _ ~ y ~ _ ~ w ~ _ ~ h => new Claim(id, x, y, w, h)
  }
}

class Claim(id: String, x: Int, y: Int, w: Int, h: Int) {
  val X: Int = x
  val Y: Int = y
  val W: Int = w
  val H: Int = h
  override def toString: String = {
    id + " @ " + x + "," + y + ": " + w + "x" + h
  }
}

object Day3 extends ClaimParser {
  def main(args: Array[String]): Unit = {
    val fabric: Array[Array[Int]] = Array.ofDim(1000, 1000)
    var overlaps = 0
    Source.fromFile("day/3/input").getLines().map(line => {
      parse(claim, line).get
    }).foreach(c => {
      for (x <- c.X until c.X + c.W; y <- c.Y until c.Y + c.H) {
        val hits = fabric(x)(y)
        if (hits == 1) {
          overlaps += 1
        }
        fabric(x)(y) = hits + 1
      }
    })
    println("fabric map:")
    for (x <- 0 until 1000) {
      for (y <- 0 until 1000) {
        print(fabric(x)(y))
      }
      println
    }
    println("number of overlaps", overlaps)
  }
}

object Day3Part2 extends ClaimParser {
  def main(args: Array[String]): Unit = {
    val fabric: Array[Array[Claim]] = Array.ofDim(1000, 1000)
    val nonOverlapping = new mutable.HashSet[Claim]
    Source.fromFile("day/3/input").getLines().map(line => {
      parse(claim, line).get
    }).foreach(c => {
      var collision = false
      for (x <- c.X until c.X + c.W; y <- c.Y until c.Y + c.H) {
        val existing = fabric(x)(y)
        if (existing == null) {
          fabric(x)(y) = c
        } else {
          collision = true
          nonOverlapping.remove(c)
          nonOverlapping.remove(existing)
        }
      }
      if (!collision) {
        nonOverlapping.add(c)
      }
    })
    println("non-overlapping", nonOverlapping.toList)
  }
}