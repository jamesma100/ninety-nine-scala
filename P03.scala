object P03 {
  def nth[A](n: Int, li:List[A]): Option[A] = {
    li match {
      case Nil => None
      case x :: rest if n == 1 => Some(x)
      case x :: rest if n > 1 => nth(n - 1, rest)
      case _ => None
    }
  }
}
