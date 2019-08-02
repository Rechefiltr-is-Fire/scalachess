package draughts

/**
 * Utility methods for helping to determine when a situation is a draw.
 *
 * De partij eindigt in remise als:
 * X a. de spelers dit met elkaar overeenkomen, of
 *   b. beide spelers vijf zetten hebben gespeeld nadat een stand is ontstaan van één dam tegen
 * hoogstens twee stukken waaronder een dam, of
 *   c. beide spelers zestien zetten hebben gespeeld nadat een stand is ontstaan van één dam tegen
 * drie stukken waaronder een dam, of
 * X d. beide spelers vijfentwintig opeenvolgende zetten alleen met dammen hebben geschoven en
 * niet hebben geslagen, of
 * X e. er voor de derde keer dezelfde stand, met dezelfde speler aan zet, op het bord is ontstaan.
 *
 */
object InsufficientWinningMaterial {

  /*
   * Determines whether a board position is an automatic draw after a certain amount of moves due to neither player
   * being able to win as informed by the traditional draughts rules material combinations (rules b and c).
   */
  def apply(board: Board): Option[Int] =
    if (board.variant.frisianVariant) {
      if (board.pieces.size <= 3 && board.roleCount(Man) == 0) {
        if (board.pieces.size == 3) Some(14)
        else Some(4)
      } else None
    } else if (board.variant.antidraughts || board.variant.breakthrough)
      None
    else if (board.pieces.size <= 4) {

      val whitePieces = board.piecesOf(Color.White)
      val blackPieces = board.piecesOf(Color.Black)

      if (whitePieces.size == 1 && whitePieces.count(_._2.role == King) == 1) {

        if (blackPieces.count(_._2.role == King) == 0) Some(50)
        else if (blackPieces.size <= 2) Some(10)
        else Some(32)

      } else if (blackPieces.size == 1 && blackPieces.count(_._2.role == King) == 1) {

        if (whitePieces.count(_._2.role == King) == 0) Some(50)
        else if (whitePieces.size <= 2) Some(10)
        else Some(32)

      } else Some(50)

    } else Some(50)

}
