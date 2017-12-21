package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
      def countParan(): Int = {
        var openParanBalance = 0
        var idx = 0
        while (idx < chars.size) {
          if (chars(idx) == '(') {
            openParanBalance += 1 
          } else if (chars(idx) == ')') {
            if (openParanBalance > 0) 
              openParanBalance -= 1 
            else {
              openParanBalance = -1
              idx = chars.size
            }
          } 
          idx += 1
        }
        openParanBalance
      }
      countParan() == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, openParanCount: Int, closeParanCount: Int) : Int = {
      var opc = openParanCount
      var cpc = closeParanCount
      var i = idx
      while (i < until) {
        if (chars(idx) == '(') opc += 1
        else if (chars(idx) == ')') cpc += 1
        i += 1
      }
      opc - cpc
    }

    def reduce(from: Int, until: Int) : Int = {
      if ((until - from) <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = (from + until) / 2
        val (l1, l2) = parallel(reduce(from, mid), reduce(mid, until)) 
        (l1 + l2)
      }
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
