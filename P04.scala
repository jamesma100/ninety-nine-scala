object P04 {
  def lengthHelper[A](li: List[A], cur: Int): Int = {
    li match {
      case Nil => 0
      case x :: Nil => cur + 1
      case x :: rest => lengthHelper(rest, cur + 1)
    }
  }
  def length[A](li: List[A]): Int = {
    lengthHelper(li, 0)
  }
}
