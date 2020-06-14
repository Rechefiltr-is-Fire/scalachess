package draughts
package variant

case object Antidraughts extends Variant(
  id = 6,
  gameType = 98,
  key = "antidraughts",
  name = "Antidraughts",
  shortName = "Anti",
  title = "Lose all your pieces (or run out of moves) to win the game.",
  standardInitialPosition = true,
  boardSize = Board.D100
) {

  def pieces = Standard.pieces
  def initialFen = Standard.initialFen
  def startingPosition = Standard.startingPosition

  def captureDirs = Standard.captureDirs
  def moveDirsColor = Standard.moveDirsColor
  def moveDirsAll = Standard.moveDirsAll

  // Only difference is that you win when you run out of moves (no pieces or all blocked)
  override def winner(situation: Situation): Option[Color] =
    if (situation.checkMate) Some(situation.color) else None

  def maxDrawingMoves(board: Board): Option[Int] = None

  /**
   * Update position hashes for threefold repetition. Clear after non-kingmove, capture or promotion.
   */
  def updatePositionHashes(board: Board, move: Move, hash: draughts.PositionHash): PositionHash = {
    val newHash = Hash(Situation(board, !move.piece.color))
    if (move.captures || move.piece.isNot(King) || move.promotes) newHash
    else newHash ++ hash
  }

}
