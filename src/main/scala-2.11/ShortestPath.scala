object ShortestPath {
  //頂点
  val vertexes='A'to'N'

  //辺
  val edges=Seq(
    Edge('A','B',9),
    Edge('A','D',6),
    Edge('A','C',6),
    Edge('B','E',2),
    Edge('C','E',9),
    Edge('C','G',6),
    Edge('D','F',3),
    Edge('E','I',1),
    Edge('F','H',5),
    Edge('F','J',9),
    Edge('G','J',9),
    Edge('G','I',3),
    Edge('H','K',5),
    Edge('J','K',4),
    Edge('J','M',6),
    Edge('J','L',7),
    Edge('K','M',1),
    Edge('L','N',3),
    Edge('M','N',2),
  )

  def solvedByDijkstra(start:Char,goal:Char):Unit={
    var distances =vertexes.map(v=>(v -> Int.MaxValue)).toMap
    var Visited=edges.map(v=>(v-> false)).toMap
    distances=distances+(start->0)
    var isUpdated=true;
    while(isUpdated){
      isUpdated=false
      edges.foreach(e=>
      if(
        !Visited(e) && distances(e.from)!=Int.MaxValue && distances(e.to)>distances(e.from)+e.distance
      ){
        distances=distances+(e.to->(distances(e.from)+e.distance))
        isUpdated=true
        Visited=Visited+(e->true)
      }
      )
    }
    println(distances)
    println(distances(goal))
  }
}