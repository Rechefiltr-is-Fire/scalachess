package draughts
package format

import variant.{ Variant, Standard }

/**
 * Transform a game to draughts Forsyth Edwards Notation
 * https://en.wikipedia.org/wiki/Portable_Draughts_Notation
 * Additions:
 * Piece role G/P = Ghost man or king of that color, has been captured but not removed because the forced capture sequence is not finished yet
 * ":Hx" = Halfmove clock: This is the number of halfmoves since a forced draw material combination appears. This is used to determine if a draw can be claimed.
 * ":Fx" = Fullmove number: The number of the full move. It starts at 1, and is incremented after Black's move.
 */
object Forsyth {

  val initial = "W:W31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50:B1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20:H0:F1"
  val initialPieces = "W31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50:B1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20"
  val initialMoveAndPieces = "W:W31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50:B1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20"

  def <<@(variant: Variant, rawSource: String): Option[Situation] = read(rawSource) { fen =>
    makeBoard(variant, fen) map { board =>

      val situation = Color.apply(fen.charAt(0)) match {
        case Some(color) => Situation(board, color)
        case _ => Situation(board, White)
      }

      situation withHistory {
        val history = DraughtsHistory(
          positionHashes = Array.empty,
          variant = variant
        )
        if (variant.frisianVariant) {
          val kingMoves = fen.split(':').lastOption.flatMap(makeKingMoves)
          kingMoves.fold(history)(history.withKingMoves)
        } else history
      }

    }
  }

  def <<(rawSource: String): Option[Situation] = <<@(Standard, rawSource)

  case class SituationPlus(situation: Situation, fullMoveNumber: Int) {

    def turns = fullMoveNumber * 2 - (if (situation.color.white) 2 else 1)

  }

  def <<<@(variant: Variant, rawSource: String): Option[SituationPlus] = read(rawSource) { source =>
    <<@(variant, source) map { sit =>
      val splitted = source.split(':')
      val fullMoveNumber = splitted find { s => s.length > 1 && s.charAt(0) == 'F' } flatMap { s => parseIntOption(s drop 1) } map (_ max 1 min 500)
      val halfMoveClock = splitted find { s => s.length > 1 && s.charAt(0) == 'H' } flatMap { s => parseIntOption(s drop 1) } map (_ max 0 min 100)
      SituationPlus(
        halfMoveClock.map(sit.history.setHalfMoveClock).fold(sit)(sit.withHistory),
        fullMoveNumber getOrElse 1
      )
    }
  }

  def <<<(rawSource: String): Option[SituationPlus] = <<<@(Standard, rawSource)

  def makeKingMoves(str: String): Option[KingMoves] = {
    str.split('+').filter(_.nonEmpty).map(_.toList) match {
      case Array(w, b) if (w.length == 1 || w.length == 3) && (b.length == 1 || b.length == 3) =>
        for {
          white <- parseIntOption(w.head.toString) if white <= 3
          black <- parseIntOption(b.head.toString) if black <= 3
        } yield KingMoves(black, white, Pos.posAt(b.tail.mkString), Pos.posAt(w.tail.mkString))
      case _ => None
    }
  }
  /**
   * Only cares about pieces positions on the board (second and third part of FEN string)
   */
  def makeBoard(variant: Variant, rawSource: String): Option[Board] = read(rawSource) { fen =>
    val fenPieces = fen.split(':').drop(1)
    if (fenPieces.isEmpty) None
    else {
      val allFields = new scala.collection.mutable.ArrayBuffer[(Pos, Piece)]
      for (line <- fenPieces) {
        if (line.nonEmpty)
          Color.apply(line.charAt(0)).fold({}) {
            color =>
              val fields = if (line.endsWith(".")) line.substring(1, line.length - 1) else line.drop(1)
              for (field <- fields.split(',')) {
                if (field.nonEmpty)
                  field.charAt(0) match {
                    case 'K' => Pos.posAt(field.drop(1)).fold({}) { pos => allFields.+=((pos, Piece(color, King))) }
                    case 'G' => Pos.posAt(field.drop(1)).fold({}) { pos => allFields.+=((pos, Piece(color, GhostMan))) }
                    case 'P' => Pos.posAt(field.drop(1)).fold({}) { pos => allFields.+=((pos, Piece(color, GhostKing))) }
                    case _ => Pos.posAt(field).fold({}) { pos => allFields.+=((pos, Piece(color, Man))) }
                  }
              }
          }
      }
      Some(Board(allFields, variant))
    }
  }

  def countGhosts(rawSource: String): Int = read(rawSource) { fen =>
    fen.split(':').filter(_.nonEmpty).foldLeft(0) {
      (ghosts, line) =>
        Color.apply(line.charAt(0)).fold(ghosts) {
          _ =>
            line.drop(1).split(',').foldLeft(ghosts) {
              (lineGhosts, field) =>
                if (field.nonEmpty && "GP".indexOf(field.charAt(0)) != -1) lineGhosts + 1 else lineGhosts
            }
        }
    }
  }

  def countKings(rawSource: String): Int = read(rawSource) { fen =>
    fen.split(':').filter(_.nonEmpty).foldLeft(0) {
      (kings, line) =>
        Color.apply(line.charAt(0)).fold(kings) {
          _ =>
            line.drop(1).split(',').foldLeft(kings) {
              (lineKings, field) =>
                if (field.nonEmpty && field.charAt(0) == 'K') lineKings + 1 else lineKings
            }
        }
    }
  }

  def >>(situation: Situation): String = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): String = parsed match {
    case SituationPlus(situation, _) => >>(DraughtsGame(situation, turns = parsed.turns))
  }

  def >>(game: DraughtsGame): String = {
    List(
      game.player.letter.toUpper,
      exportBoard(game.board),
      "H" + game.halfMoveClock.toString,
      "F" + game.fullMoveNumber.toString
    ) ::: {
        if (game.board.variant.frisianVariant) List(exportKingMoves(game.board))
        else List()
      }
  } mkString ":"

  def exportStandardPositionTurn(board: Board, ply: Int): String = List(
    Color(ply % 2 == 0).letter,
    exportBoard(board)
  ) mkString ":"

  def exportKingMoves(board: Board) = board.history.kingMoves match {
    case KingMoves(white, black, whiteKing, blackKing) => s"+$black${blackKing.fold("")(_.toString)}+$white${whiteKing.fold("")(_.toString)}"
  }

  private implicit val posOrdering = Ordering.by[Pos, Int](_.x)

  def exportBoard(board: Board): String = {
    val fenW = new scala.collection.mutable.StringBuilder(60)
    val fenB = new scala.collection.mutable.StringBuilder(60)
    fenW.append(White.letter)
    fenB.append(Black.letter)
    for (f <- 1 to 50) {
      board(f).fold({}) { piece =>
        if (piece is White) {
          if (fenW.length > 1) fenW append ','
          if (piece isNot Man) fenW append piece.forsyth
          fenW append f
        } else {
          if (fenB.length > 1) fenB append ','
          if (piece isNot Man) fenB append piece.forsyth
          fenB append f
        }
        ()
      }
    }
    fenW append ':'
    fenW append fenB
    fenW.toString
  }

  def compressedBoard(board: Board): String = {
    // roles as numbers to prevent conflict with position piotrs
    def roleId(piece: Piece) = piece.role match {
      case Man => '1'
      case King => '2'
      case GhostMan => '3'
      case GhostKing => '4'
    }
    val fenW = new scala.collection.mutable.StringBuilder(30)
    val fenB = new scala.collection.mutable.StringBuilder(30)
    fenB.append('0')
    for (f <- 1 to 50) {
      board(f).fold({}) { piece =>
        if (piece is White) {
          if (piece isNot Man) fenW append roleId(piece)
          fenW append Pos.posAt(f).get.piotr
        } else {
          if (piece isNot Man) fenB append roleId(piece)
          fenB append Pos.posAt(f).get.piotr
        }
        ()
      }
    }
    fenW append fenB
    fenW.toString
  }

  def exportScanPosition(sit: Option[Situation]): String = sit.fold("") {
    situation =>
      val pos = new scala.collection.mutable.StringBuilder(51)
      pos.append(situation.color.letter.toUpper)

      for (f <- 1 to 50) {
        situation.board(f) match {
          case Some(Piece(White, Man)) => pos append 'w'
          case Some(Piece(Black, Man)) => pos append 'b'
          case Some(Piece(White, King)) => pos append 'W'
          case Some(Piece(Black, King)) => pos append 'B'
          case _ => pos append 'e'
        }
      }
      pos.toString
  }

  def getFullMove(rawSource: String): Option[Int] = read(rawSource) { fen =>
    fen.split(':') filter (s => s.length > 1 && s.charAt(0) == 'F') lift 0 flatMap parseIntOption
  }

  def getColor(rawSource: String): Option[Color] = read(rawSource) { fen =>
    fen lift 0 flatMap Color.apply
  }

  def getPly(rawSource: String): Option[Int] = read(rawSource) { fen =>
    getFullMove(fen) map { fullMove =>
      fullMove * 2 - (if (getColor(fen).exists(_.white)) 2 else 1)
    }
  }

  private def read[A](source: String)(f: String => A): A = f(source.replace("_", " ").trim)
}
