package trellosupport.model

import ujson.Null
import upickle.default._

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter.ISO_DATE_TIME

object Trello {

  case class Label(id: String, name: String)

  object Label {
    implicit val reader: Reader[Label] = macroR
  }

  case class Card(id: String, name: String, labels: Seq[Label])

  object Card {
    implicit val reader: Reader[Card] = macroR
    def isP1(card: Card): Boolean = card.labels.exists(_.name == "P1")
    def isP2(card: Card): Boolean = card.labels.exists(_.name == "P2")
    def isP3(card: Card): Boolean = card.labels.exists(_.name == "P3")
  }

  case class List(id: String, name: String)

  object List {
    implicit val reader: Reader[List] = macroR
  }

  case class Movement(listAfter: Option[List] = None)

  object Movement {

    implicit def OptionReader[T: Reader]: Reader[Option[T]] = reader[ujson.Value].map[Option[T]] {
      case Null    => None
      case jsValue => Some(read[T](jsValue))
    }

    implicit val movementReader: Reader[Movement] = macroR
  }

  case class Action(id: String, `type`: String, date: ZonedDateTime, data: Movement)

  object Action {

    implicit val timeReader: Reader[ZonedDateTime] =
      reader[String].map[ZonedDateTime](ZonedDateTime.parse(_, ISO_DATE_TIME))

    implicit val actionReader: Reader[Action] = macroR

    private def whenMoved(action: Action, p: Action => Boolean): Option[ZonedDateTime] =
      if (p(action)) Some(action.date)
      else None

    private def isMoveTo(action: Action, p: List => Boolean): Boolean =
      action.data.listAfter.exists(p)

    private def isMoveToDoing(action: Action): Boolean = isMoveTo(action, _.name == "Doing")

    // TODO if not moved to doing, take creation time instead. See https://help.trello.com/article/759-getting-the-time-a-card-or-board-was-created
    def whenMovedToDoing(action: Action): Option[ZonedDateTime] =
      whenMoved(action, isMoveToDoing)

    def isMoveToDone(action: Action): Boolean = isMoveTo(action, _.name.startsWith("Done "))

    def whenMovedToDone(action: Action): Option[ZonedDateTime] =
      whenMoved(action, isMoveToDone)
  }
}
