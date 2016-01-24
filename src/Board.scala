import scala.collection.mutable.ListBuffer

class Board(matrix : String) {

	private val rows = matrix.count(c => c == '|')
	private val cols = (matrix.length / rows) - 1
	private val portalHoles = matrix.count(c => c >= 'A' && c < 'X')

	private val pigs : Array[Pig] = new Array[Pig](matrix.count(c => c == 'X'))
	private val board : Array[Array[Tile.value]] = Array.ofDim[Tile.value](rows, cols)
	private val portals : Array[Portal] = new Array[Portal](portalHoles / 2)

	val state = new State(board, pigs, portals)

	var ndx = 0
	var pigNdx = 0

	println("Game board")

	for (y <- Range(0, rows)) {
		for (x <- Range(0, cols)) {
			val c = matrix.charAt(ndx)
			c match {
				case '#' => board(y)(x) = Tile.WALL
				case 'X' => board(y)(x) = Tile.PAINTED; pigs(pigNdx) = new Pig(x, y); pigNdx += 1
				case _ => {
					if (c >= 'A') {
						board(y)(x) = Tile.PORTAL

						val ndx: Int = c - 'A'
						val portal = portals(ndx)

						if (portal == null) {
							portals(ndx) = new Portal((x, y), (-1, -1))
						}
						else {
							portals(ndx) = new Portal((portal.A._1, portal.A._2), (x, y))
						}
					} else {
						board(y)(x) = Tile.EMPTY
					}
				}
			}
			ndx += 1
		}
		ndx += 1
	}

	state.out()
	var steps : ListBuffer[Step] = new ListBuffer[Step]

	steps += new Step(state, null, null)

	/**
	 * Executes steps.
	 */
	def go(): Either[Step, Int] = {
		val newsteps : ListBuffer[Step] = new ListBuffer[Step]

		steps.par.map(_.execute()).seq.foreach(steps => newsteps ++= steps)

		val completedSteps : List[Step] = newsteps.filter(step => step.state.isComplete()).toList

		steps = newsteps

		println(s"next steps ${steps.size}")

		if (completedSteps.isEmpty) {
			return Right(steps.size)
		}

		Left(completedSteps.head)
	}

}
