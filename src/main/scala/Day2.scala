import scala.io.Source

object Day2 {
  def main(args: Array[String]): Unit = {
    val result = Source.fromFile("day/2/input").getLines().map(classifyID).foldLeft((0,0)){
      (totals, checksum) => {
        (totals._1 + {if (checksum._1) 1 else 0},
          totals._2 + {if (checksum._2) 1 else 0})
      }
    }
    println(result._1 * result._2)
  }

  def classifyID(s: String): Tuple2[Boolean, Boolean] = {
    val rl = s.toList.sorted
    var (runLengths, lastRunLength) = rl.zip(rl.drop(1)).foldLeft((List[Int](), 1)){
      (acc, x) => {
        val (runLengths, runLength) = acc
        val (prev, curr) = x
        if (prev == curr) {
          (runLengths, runLength + 1)
        } else {
          (runLengths.+:(runLength), 1)
        }
      }
    }
    runLengths.+:(lastRunLength).foldLeft((false, false)) {
      (acc, x) => {
        val (has2, has3) = acc
        if (x == 2) {
          (true, has3)
        } else if (x == 3) {
          (has2, true)
        } else {
          acc
        }
      }
    }
  }
}

object Day2Part2 {
  def main(args: Array[String]): Unit = {
    val ids = Source.fromFile("day/2/input").getLines().toList
    for (i <- 0 until ids.length) {
      for (j <- i until ids.length) {
        if (offByOne(ids(i), ids(j))) {
          println(lettersInCommon(ids(i), ids(j)))
          return
        }
      }
    }
    println("not found!")
  }

  def offByOne(l: String, r: String): Boolean = {
    if (l.length != r.length) {
      return false
    }
    l.zip(r).foldLeft(0){
      (acc, x) => {
        val (lch, rch) = x
        acc + (if (lch == rch) 0 else 1)
      }
    } == 1
  }

  def lettersInCommon(l: String, r: String): String = {
    l.zip(r).foldLeft(new StringBuilder){
      (acc, x) => {
        val (lch, rch) = x
        if (lch == rch) acc.append(lch) else acc
      }
    }.toString
  }
}