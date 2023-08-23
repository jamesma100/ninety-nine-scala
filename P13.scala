/*
 * Return run-length encoding of a list
 *
 * Example:
 *  scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 *  res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
 */
object P13 {
  def encodeDirect(li: List[Any]): List[Any] = {
    li match {
      case Nil                                       => Nil
      case (a: (Int, Any)) :: Nil                    => List(a)
      case a :: Nil                                  => List((1, a))
      case (a: (Int, Any)) :: b :: tail if a(1) == b => encodeDirect((a(0) + 1, b) :: tail)
      case (a: (Int, Any)) :: b :: tail              => a :: encodeDirect(b :: tail)
      case a :: tail                                 => encodeDirect((1, a) :: tail)
    }
  }
}
