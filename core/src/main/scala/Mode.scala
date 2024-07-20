package chess

import cats.Eq
import cats.derived.*

enum Mode(val id: Int, val rated: Boolean) derives Eq:

  case Casual extends Mode(0, false)
  case Rated  extends Mode(1, true)

  val name   = toString.toLowerCase
  def casual = !rated

object Mode:

  val all = values.toList

  val byId = all.mapBy(_.id)

  def apply(id: Int): Option[Mode] = byId.get(id)

  def apply(rated: Boolean) = if rated then Rated else Casual

  val default: Mode = Casual

  def orDefault(id: Int): Mode = apply(id) | default
