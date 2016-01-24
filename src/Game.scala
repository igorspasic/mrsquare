object Game {

	def main(args: Array[String]) {
		val board = new Board(
				"..A....|" +
				"....##.|" +
				".XX....|" +
				"....#..|" +
				"..#A..#|" +
		"")

		var step : Step = null
		var loop : Boolean = true

		while (loop) {
			val result : Either[Step, Int] = board.go()

			if (result.isLeft) {
				loop = false
				step = result.left.get
			}
			else if (result.isRight) {
				if (result.right.get == 0) {
					loop = false
				}
			}
		}

		if (step == null) {
			println(s"Solution not found")
			return
		}

		println(s"Done in ${step.count} moves")

		step.state.out()
		step.solution()
	}
}
