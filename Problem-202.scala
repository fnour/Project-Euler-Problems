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

def solutionsCounts(exactBounces: Long, primeDivisors: List[Int]): Long = {
  val max = ((exactBounces + 3) / 2) - 2
  val numElements = (max - 2) / 3 + 1

  val combinations = (1 to primeDivisors.size).map { k =>
    primeDivisors
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
