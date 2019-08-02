package draughts
package format

import draughts.variant.Variant

object UciDump {

  def apply(replay: Replay): List[String] =
    replay.chronoMoves map move(replay.setup.board.variant)

  def apply(moves: Seq[String], initialFen: Option[String], variant: Variant, finalSquare: Boolean = false): Valid[List[String]] =
    if (moves.isEmpty) success(Nil)
    else Replay(moves, initialFen, variant, finalSquare) flatMap (_.valid) map apply

  def move(variant: Variant)(mod: Move): String = mod.toUci.uci

}
