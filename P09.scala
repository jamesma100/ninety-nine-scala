/*
 * Pack consecutive duplicate elements into sublists
 */
object P09 {
  def pack(li: List[Any]): List[Any] = {
    li match {
      case Nil => Nil
      case (x: List[Any]) :: y :: rest if x(0) == y => pack(List(x :+ y) ::: rest)
      case x :: y :: rest if x == y => pack(List(List(x, y)) ::: rest)
      case x :: rest => x :: pack(rest)
    }
  }
}
