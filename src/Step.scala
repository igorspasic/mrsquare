import scala.collection.mutable.ListBuffer

class Step(val state: State, val previousStep : Step, val move : Move.direction) {

	def solution(): Unit = {
		var solution : String = ""
		var step = this

		while (step.move != null) {
			solution = step.move.toString + " " + solution
			step = step.previousStep
		}

		println(solution)
		println()
	}


	val count : Int = if (previousStep == null) 0 else previousStep.count + 1

	def execute() : ListBuffer[Step] = {
		val list = ListBuffer[Step]()

		{
			val up = state.up()
			if (up != null) list += new Step(up, this, Move.UP)
		}

		{
			val down = state.down()
			if (down != null) list += new Step(down, this, Move.DOWN)
		}

		{
			val left = state.left()
			if (left != null) list += new Step(left, this, Move.LEFT)
		}

		{
			val right = state.right()
			if (right != null) list += new Step(right, this, Move.RIGHT)
		}

		list
	}

}