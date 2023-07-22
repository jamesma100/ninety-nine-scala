/*
 * P07: Flatten a nested List structure
 *
 * flatten(List(1, 2, List(4, 5))) -> List(1, 2, 4, 5)
 *
 */
object P07 {
  // Dumb recursive
  def flatten1(li: List[Any]): List[Any] = {
    li match {
      case Nil => Nil
      case (head: List[Any]) :: rest => flatten1(head) ::: flatten1(rest)
      case head :: rest => head :: flatten1(rest)
    }
  }

  // FlatMap
  def flatten2(li: List[Any]): List[Any] = li.flatMap {
    case x: List[Any] => flatten2(x)
    case x => List(x)
}
