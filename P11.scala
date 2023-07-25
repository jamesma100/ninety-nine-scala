/*
 * Modified run length encoding of a list
 * e.g. List(1, 1, 2, 2, 3, 5, 6, 6) returns List((2, 1), (2, 2), 3, 5, (2, 6))
 */
object P11 {
  // copied from P09.scala
  def pack(li: List[Any]): List[Any] = {
    li match {
      case Nil => Nil
      case (x: List[Any]) :: y :: rest if x(0) == y => pack(List(x :+ y) ::: rest)
      case x :: y :: rest if x == y => pack(List(List(x, y)) ::: rest)
      case x :: rest => x :: pack(rest)
    }
  }

  def encode_helper(li: List[Any]): List[Any] = {
    li match {
      case Nil => Nil
      case (x: List[Any]) :: rest => (x.length, x(0)) :: encode_helper(rest)
      case x :: rest => x :: encode_helper(rest)
    }
  }

  def encode(li: List[Any]): List[Any] = {
    encode_helper(pack(li))
  }
}
