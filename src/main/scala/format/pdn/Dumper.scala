package draughts
package format.pdn

object Dumper {

  def apply(situation: Situation, data: draughts.Move, next: Situation): String = data.orig.shortKey + (if (data.captures) "x" else "-") + data.dest.shortKey

  def apply(data: draughts.Move): String = apply(
    data.situationBefore,
    data,
    data.afterWithLastMove() situationOf !data.color
  )

}
