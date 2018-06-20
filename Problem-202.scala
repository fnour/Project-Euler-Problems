def solutionsCounts(exactBounces: Long): Long = {
  val max = ((exactBounces + 3) / 2) - 2
  val numElements = (max - 2) / 3 + 1
  val uniqueDivisors: List[Long] = primeDivisors(max + 2).distinct

  val combinations = (1 to uniqueDivisors.size).map { k =>
    uniqueDivisors
      .combinations(k)
      .map(_.product)
  }.map(_.toList)
  val zd = combinations.map{ list =>
    list.map(k => numOfDivisorsInASequence(exactBounces, k)).sum
  }
  val positives = zd.sliding(1,2).flatten.sum
  val negatives = zd.drop(1).sliding(1,2).flatten.sum

  numElements - (positives - negatives)
}

assert(solutionsCounts(11) == 2)
assert(solutionsCounts(1000001) == 80840)
assert(solutionsCounts(12017639147L) == 1209002624)


def numOfDivisorsInASequence(exactBounces: Long, k: Long): Long = {
  val max = (exactBounces + 3) / 2 - 2
  val numElements = (max - 2) / 3 + 1

  val indexOfFirstOccurrence = k % 3 match {
    case 0 => numElements + 2
    case 1 => (2 * k - 2) / 3 + 1
    case 2 => (k - 2) / 3 + 1
  }

  math.floor((numElements - indexOfFirstOccurrence + 1)/k.toDouble).toLong + 1
}

// TESTS for 12,017,639,147 bounces:
// BUG:
//assert(numOfDivisorsInASequence(12017639147L, 1) == 2002939858L)
assert(numOfDivisorsInASequence(12017639147L, 2) == 1001469930L)
assert(numOfDivisorsInASequence(12017639147L, 5) == 400587972L)

// TESTS for 1,000,001 bounces:
// BUG:
//assert(numOfDivisorsInASequence(1000001, 1) == 166667)
assert(numOfDivisorsInASequence(1000001, 2) == 83334)
assert(numOfDivisorsInASequence(1000001, 53) == 3145)
assert(numOfDivisorsInASequence(1000001, 89) == 1873)
assert(numOfDivisorsInASequence(1000001, 106) == 1572)
assert(numOfDivisorsInASequence(1000001, 178) == 936)
assert(numOfDivisorsInASequence(1000001, 500001) == 0)


import scala.annotation.tailrec
def primeDivisors(num: Long): List[Long] = {
  require(num > 1, "Error: input a number larger than 1")
  @tailrec
  def primeDivisorsHelper(n: Long, div: List[Long]): List[Long] = {
    if (div.product < num) {
      Stream.from(2).find(n % _ == 0) match {
        case Some(nextDivisor) => primeDivisorsHelper(n / nextDivisor, div :+ nextDivisor.toLong)
      }
    } else {
      div
    }
  }

  primeDivisorsHelper(num, List.empty[Long])
}

assert(primeDivisors(2) == List(2))
assert(primeDivisors(25) == List(5,5))
assert(primeDivisors(255) == List(3,5,17))
assert(primeDivisors(500002) == List(2,53,53,89))
assert(primeDivisors(6008819575L) == List(5,5,11,17,23,29,41,47))

