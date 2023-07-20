object P02 {
  def penultimate[A](li: List[A]): Option[A] = {
    li match {
      case Nil => None
      case a :: _ :: Nil => Some(a)
      case _ :: rest => penultimate(rest)
    }
  }
}
