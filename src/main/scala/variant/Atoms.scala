package draughts
package variant

case object Atoms extends Variant(
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

  /** If the move captures, we explode the surrounding pieces. Otherwise, nothing explodes. */
  private def explodeSurroundingPieces(move: Move): Move = {
    if (move.captures) {
      val affectedPos = surroundingPositions(move.dest)
      val afterBoard  = move.after
      val destination = move.dest

      val boardPieces = afterBoard.pieces

      // All pieces surrounding the captured piece and the capturing piece itself explode.
      val piecesToExplode = affectedPos.filter(boardPieces.get(_).fold(false)) + destination
      val afterExplosions = boardPieces -- piecesToExplode
      
      val newBoard = afterBoard withPieces afterExplosions
      move withAfter newBoard
    } else move
  }

  /** The positions surrounding a given position on the board. Any square at the edge of the board has
    * less surrounding positions than the usual four.
    */
  private[chess] def surroundingPositions(pos: Pos): Set[Pos] =
    Set(pos.upLeft, pos.upRight, pos.downLeft, pos.downRight).flatten

  override def addVariantEffect(move: Move): Move = explodeSurroundingPieces(move)

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
