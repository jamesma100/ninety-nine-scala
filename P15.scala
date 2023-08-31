/*
 * Duplicate elements of a list N times
 */
object P15 {
  def duplicateN(n: Int, li: List[Any]): List[Any] = {
    li.flatMap(i => (1 to n).map(_ => i))
  }
}
