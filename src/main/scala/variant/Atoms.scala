package draughts
package variant

case object Breakthrough extends Variant(
  id = 9,
  gameType = 96,
  key = "atoms",
  name = "Atoms",
  shortName = "Atom",
  title = "Pieces are exploding where capture.",
  standardInitialPosition = true,
  boardSize = Board.D100
) {

  def pieces = Standard.pieces
  def initialFen = Standard.initialFen
  def startingPosition = Standard.startingPosition

  def captureDirs = Standard.captureDirs
  def moveDirsColor = Standard.moveDirsColor
  def moveDirsAll = Standard.moveDirsAll



  override def winner(situation: Situation): Option[Color] =
    if (situation.checkMate) Some(!situation.color)
    else if (situation.board.kingPosOf(White).isDefined) Some(White)
    else if (situation.board.kingPosOf(Black).isDefined) Some(Black)
    else None

  def maxDrawingMoves(board: Board): Option[Int] = None

  /**
   * No drawing rules
   */
  def updatePositionHashes(board: Board, move: Move, hash: draughts.PositionHash): PositionHash =
    Hash(Situation(board, !move.piece.color))

}
