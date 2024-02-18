package chess

import Square.*
import variant.{ Antichess, Atomic, Crazyhouse, Standard, ThreeCheck }
import chess.format.{ EpdFen, Fen, Uci }
import chess.format.pgn.SanStr
import scala.util.chaining.given

class HashTest extends ChessTest:

  def hexToBytes(str: String) =
    str.grouped(2).map(cc => Integer.parseInt(cc, 16).toByte).toArray

  given munit.Compare[PositionHash, Array[Byte]] with
    def isEqual(obtained: PositionHash, expected: Array[Byte]): Boolean =
      obtained.value.toSeq == expected.toSeq
  given munit.Compare[PositionHash, PositionHash] with
    def isEqual(obtained: PositionHash, expected: PositionHash): Boolean =
      obtained.value.toSeq == expected.value.toSeq

  Hash(8).pipe: hash =>

    // Reference values available at:
    // http://hardy.uhasselt.be/Toga/book_format.html
    // https://web.archive.org/web/20191216195456/http://hardy.uhasselt.be:80/Toga/book_format.html

    test("Polyglot hasher: match on the starting position"):
      val fen  = EpdFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
      val game = fenToGame(fen, Standard)
      assertEquals(hash(game.situation), hexToBytes("463b96181691fc9c"))

    test("Polyglot hasher: match after 1. e4"):
      val fen  = EpdFen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")
      val game = fenToGame(fen, Standard)
      assertEquals(hash(game.situation), hexToBytes("823c9b50fd114196"))

    test("Polyglot hasher: match after 1. e4 d5"):
      val fen  = EpdFen("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")
      val game = fenToGame(fen, Standard)
      assertEquals(hash(game.situation), hexToBytes("0756b94461c50fb0"))

    test("Polyglot hasher: match after 1. e4 d5 2. e5"):
      val fen  = EpdFen("rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2")
      val game = fenToGame(fen, Standard)
      assertEquals(hash(game.situation), hexToBytes("662fafb965db29d4"))

    test("Polyglot hasher: match after 1. e4 d5 2. e5 f5"):
      // note that en-passant matters
      val fen  = EpdFen("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3")
      val game = fenToGame(fen, Standard)
      assertEquals(hash(game.situation), hexToBytes("22a48b5a8e47ff78"))

    test("Polyglot hasher: match after 1. e4 d5 2. e5 f5 3. Ke2"):
      // 3. Ke2 forfeits castling rights
      val fen  = EpdFen("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR b kq - 1 3")
      val game = fenToGame(fen, Standard)
      assertEquals(hash(game.situation), hexToBytes("652a607ca3f242c1"))

    test("Polyglot hasher: match after 1. e4 d5 2. e5 f5 3. Ke2 Kf7"):
      val fen  = EpdFen("rnbq1bnr/ppp1pkpp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR w - - 2 4")
      val game = fenToGame(fen, Standard)
      assertEquals(hash(game.situation), hexToBytes("00fdd303c946bdd9"))

    test("Polyglot hasher: match after 1. a4 b5 2. h4 b4 3. c4"):
      // again, note en-passant matters
      val fen  = EpdFen("rnbqkbnr/p1pppppp/8/8/PpP4P/8/1P1PPPP1/RNBQKBNR b KQkq c3 0 3")
      val game = fenToGame(fen, Standard)
      assertEquals(hash(game.situation), hexToBytes("3c8123ea7b067637"))

    test("Polyglot hasher: match after 1. a4 b5 2. h4 b4 3. c4 bxc3 4. Ra3"):
      // 4. Ra3 partially forfeits castling rights
      val fen  = EpdFen("rnbqkbnr/p1pppppp/8/8/P6P/R1p5/1P1PPPP1/1NBQKBNR b Kkq - 1 4")
      val game = fenToGame(fen, Standard)
      assertEquals(hash(game.situation), hexToBytes("5c3f9b829b279560"))

  Hash(3).pipe: hash =>

    test("Hasher: account for checks in three-check"):
      // 2 ... Bb4+
      val gameA = Game(Board init ThreeCheck)
        .playMoves(
          E2 -> E4,
          E7 -> E6,
          D2 -> D4,
          F8 -> B4
        )
        .toOption
        .get

      // repeat
      val gameB = gameA
        .playMoves(
          C1 -> D2,
          B4 -> F8,
          D2 -> C1,
          F8 -> B4
        )
        .toOption
        .get

      assertNotEquals(hash(gameA.situation), hash(gameB.situation))

    test("Hasher: account for pockets in crazyhouse"):
      val gameA = Game(Crazyhouse)
        .playMoves(E2 -> E4, D7 -> D5, E4 -> D5)
        .get

      val intermediate = Game(Crazyhouse)
        .playMoves(E2 -> E4, D7 -> D5, E4 -> D5, D8 -> D7)
        .get

      // we reach the same position, but now the pawn is in blacks pocket
      val gameB = intermediate(Uci.Drop(Pawn, D6)).get._1
        .playMoves(
          D7 -> D6,
          D1 -> E2,
          D6 -> D8,
          E2 -> D1
        )
        .get

      assertNotEquals(hash(gameA.situation), hash(gameB.situation))

    test("Hasher: be consistent in crazyhouse"):
      // from https://lichess.org/j4r7XHTB/black
      val fen           = EpdFen("r2qkb1r/ppp1pppp/2n2n2/3p2B1/3P2b1/4PN2/PPP1BPPP/RN1QK2R/ b KQkq - 9 5")
      val situation     = Fen.read(Crazyhouse, fen).get
      val move          = situation.move(Square.G4, Square.F3, None).get
      val hashAfterMove = hash(move.situationAfter)

      // 5 ... Bxf3
      val fenAfter       = EpdFen("r2qkb1r/ppp1pppp/2n2n2/3p2B1/3P4/4Pb2/PPP1BPPP/RN1QK2R/n w KQkq - 10 6")
      val situationAfter = Fen.read(Crazyhouse, fenAfter).get
      val hashAfter      = hash(situationAfter)

      assertEquals(hashAfterMove, hashAfter)

    test("Hasher: be consistent when king is captured in antichess"):
      val fen           = EpdFen("rnbqkb1r/ppp1pppp/3p1n2/1B6/8/4P3/PPPP1PPP/RNBQK1NR w KQkq - 2 3")
      val situation     = Fen.read(Antichess, fen).get
      val move          = situation.move(Square.B5, Square.E8, None).get
      val hashAfterMove = hash(move.situationAfter)

      // 3. BxK
      val fenAfter       = EpdFen("rnbqBb1r/ppp1pppp/3p1n2/8/8/4P3/PPPP1PPP/RNBQK1NR b KQkq - 0 3")
      val situationAfter = Fen.read(Antichess, fenAfter).get
      val hashAfter      = hash(situationAfter)

      assertEquals(hashAfterMove, hashAfter)

    test("Hasher: be consistent when rook is exploded in atomic"):
      val fen           = EpdFen("rnbqkb1r/ppppp1pp/5p1n/6N1/8/8/PPPPPPPP/RNBQKB1R w KQkq - 2 3")
      val situation     = Fen.read(Atomic, fen).get
      val move          = situation.move(Square.G5, Square.H7, None).get
      val hashAfterMove = hash(move.situationAfter)

      // 3. Nxh7
      val fenAfter       = EpdFen("rnbqkb2/ppppp1p1/5p2/8/8/8/PPPPPPPP/RNBQKB1R b KQkq - 0 3")
      val situationAfter = Fen.read(Atomic, fenAfter).get
      val hashAfter      = hash(situationAfter)

      assertEquals(hashAfterMove, hashAfter)

    test("Hasher: prod 5 Three-Check games accumulate hash"):
      val gameMoves = format.pgn.Fixtures.prod5threecheck.map { g =>
        SanStr from g.split(' ').toList
      }
      def runOne(moves: List[SanStr]) =
        Replay.gameMoveWhileValid(moves, Fen.initial, chess.variant.ThreeCheck)

      def hex(buf: Array[Byte]): String = buf.map("%02x" format _).mkString
      val g                             = gameMoves.map(runOne)
      assertNot(g.exists(_._3.nonEmpty))
      val m16 = java.security.MessageDigest getInstance "MD5"
      val h   = Hash(16)
      g.foreach(_._2.foreach(x => m16.update(PositionHash value h(x._1.situation))))
      assertEquals(hex(m16.digest), "21281304d25ccf9c1dfd640775800087")

  test("Index out of bounds when hashing pockets"):
    val fenPosition = EpdFen("2q1k1nr/B3bbrb/8/8/8/8/3qN1RB/1Q2KB1R/RRRQQQQQQrrrqqq w Kk - 0 11")
    val game        = fenToGame(fenPosition, Crazyhouse)
    assert(game.apply(E1, D2).isRight)
