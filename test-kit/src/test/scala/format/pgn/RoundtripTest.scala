package chess
package format.pgn

import scala.language.implicitConversions

class RoundtripTest extends ChessTest:

  test("roundtrip with special chars for tags"):
    val value = "aä\\\"'$%/°á \t\b \"\\\\/"
    val parsed = Parser
      .full(Pgn[Move](tags = Tags(List(Tag(_.Site, value))), InitialComments.empty, None).render)
      .toOption
      .get
    assertEquals(parsed.tags("Site"), Some(value))
