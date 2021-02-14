case class Edge(from: Char, to: Char, distance: Int)

object ShortestPath {

  /**
   * 頂点
   */
  val vertexes = 'A' to 'N'

  /**
   * 辺
   */
  val edges = Seq(
    Edge('A', 'B', 9),
    Edge('A', 'C', 6),
    Edge('A', 'D', 6),
    Edge('B', 'A', 9),
    Edge('B', 'E', 2),
    Edge('C', 'A', 6),
    Edge('C', 'E', 9),
    Edge('C', 'G', 6),
    Edge('D', 'A', 6),
    Edge('D', 'F', 3),
    Edge('E', 'B', 2),
    Edge('E', 'C', 9),
    Edge('E', 'I', 1),
    Edge('F', 'D', 3),
    Edge('F', 'H', 5),
    Edge('F', 'J', 9),
    Edge('G', 'C', 6),
    Edge('G', 'I', 3),
    Edge('G', 'J', 9),
    Edge('H', 'F', 5),
    Edge('H', 'K', 5),
    Edge('I', 'E', 1),
    Edge('I', 'G', 3),
    Edge('J', 'F', 9),
    Edge('J', 'G', 9),
    Edge('J', 'K', 4),
    Edge('J', 'L', 7),
    Edge('J', 'M', 6),
    Edge('K', 'H', 5),
    Edge('K', 'J', 4),
    Edge('K', 'M', 1),
    Edge('L', 'J', 7),
    Edge('L', 'N', 3),
    Edge('M', 'J', 6),
    Edge('M', 'K', 1),
    Edge('M', 'N', 2),
    Edge('N', 'L', 3),
    Edge('N', 'M', 2)
  )

  def solveByBellmanFord(start: Char, goal: Char): Unit = {
    // 各頂点までの距離の初期化
    var distances = vertexes.map(v => (v -> Int.MaxValue)).toMap
    distances = distances + (start -> 0)

    var isUpdated = true
    while (isUpdated) {
      isUpdated = false
      edges.foreach { e =>
        if(distances(e.from) != Int.MaxValue
          && distances(e.to) > distances(e.from) + e.distance) {
          distances = distances + (e.to -> (distances(e.from) + e.distance))
          isUpdated = true
        }
      }
    }

//    println(distances)
//    println(distances(goal))
  }

  def solveByDijkstra(start: Char, goal: Char, _edges: Seq[Edge]= edges): Unit = {
    // 初期化
    var distances = vertexes.map(v => (v -> Int.MaxValue)).toMap
    distances = distances + (start -> 0)
    var edges = _edges
    var trash: Set[Edge] = Set()
    var isUpdated = true

    while (isUpdated) {
      isUpdated = false
      edges.foreach { e =>
        if(distances(e.from) != Int.MaxValue) {
          if (distances(e.to) > distances(e.from) + e.distance)
          {
            distances = distances + (e.to -> (distances(e.from) + e.distance))
            isUpdated = true
          }
          trash = trash + e
        }
      }
      edges = edges.filterNot(trash contains( _))
    }

//    println(distances)
//    println(distances(goal))
  }

  def speedTest(): Unit = {
    val start = vertexes.head
    val goal = vertexes.last
    val repeat = 1000
    var results = Seq.empty[Long]
    var startTime: Long = 0
    var endTime: Long = 0

    startTime = System.currentTimeMillis
    for (i <- 0 until repeat) solveByBellmanFord(start, goal)
    endTime = System.currentTimeMillis - startTime
    results = results :+ endTime

    startTime = System.currentTimeMillis
    for (i <- 0 until repeat) solveByDijkstra(start, goal)
    endTime = System.currentTimeMillis - startTime
    results = results :+ endTime

    println(s"BellmanFord: ${results(0)}")
    println(s"Dijkstra: ${results(1)}")

  }

}