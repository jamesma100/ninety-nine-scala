/*
 * Eliminate consecutive duplicates
 */
object P08 {
  def compress[A](li: List[A]): List[A] = {
    li match {
      case Nil => Nil
      case x :: Nil => List(x)
      case x :: y :: rest if x == y => compress(x :: rest)
      case x :: rest => x :: compress(rest)
    }
  }
}
