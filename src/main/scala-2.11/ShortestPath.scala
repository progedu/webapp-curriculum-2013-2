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

    // 各頂点までの距離と直前の頂点の初期化 Map[node:Char -> (distance: Int, prevNode: Char)]    
    var nodes: Map[Char, (Int, Char)] = vertexes.map(v => v -> (Int.MaxValue, '_')).toMap
    nodes = nodes + (start -> (0, '_'))

    // キュー
    var queueEdges = edges        // Seq(Edge): 辺、前後の頂点とその頂点間のの距離を保持
    var queueNodes = nodes.keys   // List[Char]: 各頂点のノード名を保持

    // 本計算
    // キュー が空になるまで
    while (queueEdges.nonEmpty) {
      // d(u) が最小である頂点uを見つける
      var u = '_'
      var minVal = Int.MaxValue
      queueNodes.foreach {e =>
        val (distance, _) = nodes(e)
        // より小さい距離を保持する頂点を見つけたら、そこが始点uになるように更新
        if (minVal > distance) {
          minVal = distance
          u = e
        }
      }
      
      val subQueue = queueEdges.filter(_.from == u)　// キュー から d(u) が最小である頂点uを取り出す
      queueEdges = queueEdges.filter(_.from != u)   // キューを更新：頂点uを始点とする辺をキューから取り除く
      queueNodes = queueNodes.filter(k => k != u)   // キューを更新：頂点uのノード名をキューから取り除く


      // for each ( u から辺が伸びている各頂点 v )
      subQueue.foreach { e =>
        val (uDistance, _) = nodes(e.from)    // 始点となる頂点uまでの距離
        val (vDistance, _) = nodes(e.to)      // 終点となる頂点vまでの距離
        // if ( d(v) > d(u) + length(u,v) )
        if (vDistance > (uDistance + e.distance))
          nodes = nodes + (e.to -> (uDistance + e.distance, e.from)) // distanceとprevの更新
      }

    }
    // 探索の結果表示
    println(nodes)

    // スタートからゴールまでの経路を取得
    var paths: Seq[Char] = Seq()  
    var path = goal
    while(path != '_') {
      paths = paths :+ path
      val (_ , prev) = nodes(path)

      path = prev
    }
    paths = paths.reverse

    // スタートからゴールまでの経路表示
    println(paths)
  }
}