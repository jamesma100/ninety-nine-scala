/*
 * P05: Reverse a list
 */
object P05 {
  // Simple recursive
  def reverse1[A](li: List[A]): List[A] = {
    li match {
      case Nil => Nil
      case x :: Nil => List(x)
      case x :: rest => reverse1(rest) ::: List(x)
    }
  }

  // Tail recursive
  def reverse2[A](li: List[A]): List[A] = {
    def reverseTail(li: List[A], sol: List[A]): List[A] = {
      li match {
        case Nil => sol
        case x :: rest => reverseTail(rest, List(x) ::: sol)
      }
    }
    reverseTail(li, Nil)
  }

  // Fold
  def reverse3[A](li: List[A]): List[A] = {
    li.foldRight(Nil) {
      (a: A, b: List[A]) => {
        b :+ a
      }
    }
  }
}
