case class State(board : Array[Array[Tile.value]], pigs : Array[Pig], portals : Array[Portal]) {

	def out() = {
		println()
		for(array <- board) {
			for(tile <- array) {
				tile match {
					case Tile.EMPTY => print('.')
					case Tile.WALL => print('#')
					case Tile.PAINTED => print('*')
					case Tile.PORTAL => print('@')
				}
			}
			println()
		}
		println()
	}

	/**
	 * Checks if board is completely painted.
	 */
	def isComplete: Boolean = {
		for(array <- board) {
			for(tile <- array) {
				if (tile == Tile.EMPTY) {
					return false
				}
			}
		}
		true
	}

	/**
	 * Clones state.
	 */
	private def newState() : State = {
		val newBoard = new Array[Array[Tile.value]](board.length)

		for (i <- Range(0, board.length)) {
			newBoard(i) = board(i).clone()
		}

		val newPigs : Array[Pig] = pigs
			.map(pig => new Pig(pig.x, pig.y))
			.toList
			.toArray

		new State(newBoard, newPigs, portals)
	}

	private def isOk(state : State, x : Int, y : Int) : Boolean ={
		val tile = state.board(y)(x)

		tile match {
			case Tile.EMPTY => true
			case Tile.PORTAL => true
			case _ => false
		}
	}

	private def portal(state : State, x: Int, y: Int): Option[Portal] = {
		for (portal <- portals) {
			val tile = state.board(y)(x)
			if (portal.onit(x,y)) {
				return Option[Portal](portal)
			}
		}
		Option.empty
	}

	private def onmove(state : State, x: Int, y: Int) : (Int, Int) = {
		val p = portal(state, x, y)
		if (p.isDefined) {
			return p.get.jump(x, y)
		}
		state.board(y)(x) = Tile.PAINTED
		(x,y)
	}


	def up() : State = {
		move((state,xx,yy) => {
			var y = yy - 1
			var x = xx
			while ((y >= 0) && isOk(state, x, y)) {
				val m = onmove(state, x, y)
				x = m._1
				y = m._2

				y -= 1
			}
			(x, y + 1)
		})
	}

	def down() : State = {
		move((state,xx,yy) => {
			var x = xx
			var y = yy + 1
			while ((y < state.board.length) && isOk(state, x, y)) {
				val m = onmove(state, x, y)
				x = m._1
				y = m._2

				y += 1
			}
			(x, y - 1)
		})
	}

	def left() : State = {
		move((state,xx,yy) => {
			var x = xx - 1
			var y = yy
			while ((x >= 0) && isOk(state, x, y)) {
				val m = onmove(state, x, y)
				x = m._1
				y = m._2

				x -= 1
			}
			(x + 1, y)
		})
	}

	def right() : State = {
		move((state,xx,yy) => {
			var x = xx + 1
			var y = yy
			while ((x < state.board(y).length) && isOk(state, x, y)) {
				val m = onmove(state, x, y)
				x = m._1
				y = m._2

				x += 1
			}
			(x - 1, y)
		})
	}


	private def move(f : (State, Int, Int) => (Int, Int)) : State = {
		val state = newState()
		var anyPigMoved : Boolean = false

		for (i <- state.pigs.indices) {
			val pig = state.pigs(i)

			val xy = f(state, pig.x, pig.y)

			if ((xy._1 != pig.x) || (xy._2 != pig.y)) {
				anyPigMoved = true
				state.pigs(i) = new Pig(xy._1, xy._2)
			}
		}

		if (!anyPigMoved) {
			return null
		}

		state
	}
}