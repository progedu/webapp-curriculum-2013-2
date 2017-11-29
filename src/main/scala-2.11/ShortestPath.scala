import scala.collection.mutable.PriorityQueue

/**
  * Created by soichiro_yoshimura on 2016/06/27.
  */
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

    println(distances)
    println(distances(goal))
  }

  def solveByDijkstra(start: Char, goal: Char): Unit = {
    // 各頂点までの距離の初期化
    var distances = vertexes.map(v => (v -> Int.MaxValue)).toMap
    var flags = vertexes.map(v => (v -> false)).toMap
    distances = distances updated(start, 0)

    var q = new PriorityQueue[(Int, Char)]
    q.enqueue((0, start))

    while (!q.isEmpty) {
      val (dist, from) = q.head
      q.dequeue()
      flags = flags updated(from, true)

      if (!(distances(from) < dist * (-1))) {
        for (e <- edges if e.from == from) {
          val upDist = distances(from) + e.distance
          if (!flags(e.to) && distances(e.to) > upDist) {
            distances = distances updated(e.to, upDist)
            q.enqueue((distances(e.to) * -1, e.to))
          }
        }
      }
    }

    println(distances)
    println(distances(goal))
  }
}
