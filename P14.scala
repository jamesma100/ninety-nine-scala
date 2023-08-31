/*
 * Duplicate elements in a list
 */
object P14 {
  def duplicate(li: List[Any]): List[Any] = {
    li.flatMap(i => List(i, i))
  }
}
