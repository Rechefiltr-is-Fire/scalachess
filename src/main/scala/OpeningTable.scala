package draughts

case class OpeningTable(key: String, name: String, url: String, positions: List[StartingPosition]) {

  lazy val shuffled = new scala.util.Random(475592).shuffle(positions).toIndexedSeq

  def randomOpening: (Int, StartingPosition) = {
    val index = scala.util.Random.nextInt(shuffled.size)
    index -> shuffled(index)
  }
}

object OpeningTable {

  import StartingPosition.Category

  val categoriesIDF = List(
    Category("I", List(
      StartingPosition("I.1", "B:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12:H0:F1", "1. ab4", Some("ab4"))
    ))
  )

  val tableIDF = OpeningTable(
    key = "idf",
    name = "IDF competitions Table of draw",
    url = "https://idf64.org/tables-of-draw/",
    positions = categoriesIDF.flatMap(_.positions)
  )

  val allTables = List(tableIDF)

  def byKey = key2table.get _
  private val key2table: Map[String, OpeningTable] = allTables.map { p =>
    p.key -> p
  }(scala.collection.breakOut)

}