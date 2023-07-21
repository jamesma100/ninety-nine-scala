/*
 * P04: Find length of list
 */
object P04 {
  def length[A](li: List[A]): Int = {
    def lengthHelper[A](li: List[A], cur: Int): Int = {
      li match {
        case Nil => 0
        case x :: Nil => cur + 1
        case x :: rest => lengthHelper(rest, cur + 1)
      }
    }
    lengthHelper(li, 0)
  }
}
