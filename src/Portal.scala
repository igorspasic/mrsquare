case class Portal(A : (Int, Int), B : (Int, Int)) {

	def jump(x: Int, y: Int) : (Int, Int) = {
		if (A._1 == x && A._2 == y) {
			return B
		}
		if (B._1 == x && B._2 == y) {
			return A
		}
		null
	}

	def onit(x: Int, y: Int): Boolean = {
		if ((A._1 == x && A._2 == y) || (B._1 == x && B._2 == y)) {
			return true
		}
		false
	}
}
