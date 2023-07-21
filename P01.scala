/*
 * P01: Find last element of a list
 */
object P01 {
  def last[A](li: List[A]): Option[A] = {
    li match {
      case List() => None
      case List(x) => Some(x)
      case first :: rest => last(rest)
    }
  }
}
