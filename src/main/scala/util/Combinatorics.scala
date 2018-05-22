package util

object Combinatorics {
  def splits(i: Int, N: Int, M: Int): Seq[List[Double]] = if (i < N) {
    // i: current dimension
    // m: remaining budget (excluding corner points)
    val default = {
      for (p <- Seq(0.0, 1.0); ps <- splits(i + 1, N, M)) yield {
        p :: ps
      }
    }

    val extra = (1 to M) flatMap {
      k =>
        val d = (1 << k) // denominator
        for (n <- 1 to d if n % 2 == 1; ps <- splits(i + 1, N, M - k)) yield {
          val p = ((n: Double) / d)
          p :: ps
        }
    }

    default ++ extra
  } else if (M == 0) {
    Seq(Nil)
  } else {
    Seq()
  }
}