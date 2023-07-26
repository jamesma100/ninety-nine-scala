/*
 * Given a run length encoding, decode it
 * List((2, 'a'), (1, 'b'), (3, 'c')) => List('a', 'a', 'b', 'c', 'c', 'c')
 */ 
object P12 {
	def decode(li: List[(Int, Any)]): List[Any] = {
		li match {
			case Nil => Nil
			case (head: (Int, Any)) :: tail if head(0) == 1 => List(head(1)) ::: decode(tail)
			case (head: (Int, Any)) :: tail => head(1) :: decode(List((head(0) - 1, head(1)))) ::: decode(tail)  
		}
	}
}
