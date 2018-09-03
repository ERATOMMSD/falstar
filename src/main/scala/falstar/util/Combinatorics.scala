package falstar.util

object Combinatorics {
  def splits[A](as: List[A], M: Int): Seq[Map[A, Double]] = as match {
    case a :: as =>
      // i: current dimension
      // N: number of dimensions
      // M: remaining budget (excluding corner points)

      // corners are included by default
      val default = {
        for (p <- Seq(0.0, 1.0); ps <- splits(as, M)) yield {
          ps + (a -> p)
        }
      }

      val extra = (1 to M) flatMap {
        k =>
          val d = (1 << k) // denominator 2^k
          for (n <- 1 to d if n % 2 == 1; ps <- splits(as, M - k)) yield {
            val p = ((n: Double) / d)
            ps + (a -> p)
          }
      }

      default ++ extra
    case Nil =>
      if (M == 0) // budget M exhausted -> return a single result
        Seq(Map())
      else //budget not exhausted -> don't return any results and cause the entire choice to disappear
        Seq()
  }

  def main(args: Array[String]) {
    println(splits(List("a", "b"), 3))
  }
}