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
    def balanceAcc(acc: Int, chars: List[Char]): Boolean = (acc, chars) match {
      case (n, Nil) => n == 0
      case (n, _) if n < 0 => false
      case (n, '(' :: xs) => balanceAcc(n + 1, xs)
      case (n, ')' :: xs) => balanceAcc(n - 1, xs)
      case (n, x :: xs) => balanceAcc(n, xs)
    }

    balanceAcc(0, chars.toList)
  }


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    /**
      *

)()()( -> (1,1)
)(() -> (1,1)
))(
      ))( ->
      * @return
      */
    def traverse(from: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      var i = from
      var closing = 0
      var opening = 0
      var lastOpened = false
      while (i<until) {

        i = i+1
      }

      (closing, opening)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from < threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = (until + from) / 2
        val ((l1, r1), (l2, r2)) = parallel(
          reduce(from, mid),
          reduce(mid, until)
        )
        // TODO cases?
        (l1, r2)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
