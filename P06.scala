/*
 * Determine whether a list is a palindrome
 */
object P06 {
  // Simple foldLeft
  def isPalindrome1[A](li: List[A]): Boolean = {
    li == li.foldLeft(Nil) {
      (a: List[A], b: A) => b :: a
    }
  }
}
