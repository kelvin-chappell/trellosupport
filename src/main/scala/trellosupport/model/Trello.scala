package trellosupport.model

import ujson.Null
import upickle.default._

import java.time.{Instant, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter.ISO_DATE_TIME

object Trello {

  case class Label(id: String, name: String)

  object Label {
    implicit val reader: Reader[Label] = macroR
  }

  case class Card(id: String, idList: String, name: String, labels: Seq[Label])

  object Card {
    implicit val reader: Reader[Card] = macroR

    private def hasLabel(card: Card, p: Label => Boolean): Boolean = card.labels.exists(p)
    private def hasPriority(card: Card, priority: Int): Boolean =
      hasLabel(card, _.name == s"P$priority")
    def isP1(card: Card): Boolean = hasPriority(card, 1)
    def isP2(card: Card): Boolean = hasPriority(card, 2)
    def isP3(card: Card): Boolean = hasPriority(card, 3)

    // See https://help.trello.com/article/759-getting-the-time-a-card-or-board-was-created
    def whenCreated(card: Card): ZonedDateTime = {
      val epochSecond = Integer.parseInt(card.id.take(8), 16)
      ZonedDateTime.ofInstant(Instant.ofEpochSecond(epochSecond), ZoneId.of("UTC"))
    }
  }

  case class List(id: String, name: String)

  object List {
    implicit val reader: Reader[List] = macroR
  }

  case class Movement(listBefore: Option[List] = None, listAfter: Option[List] = None)

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

    private def isMoveToDoingList(action: Action): Boolean = isMoveTo(action, _.name == "Doing")

    def isMoveToDoneList(action: Action): Boolean = isMoveTo(action, _.name.startsWith("Done "))

    def whenMovedToDoingList(action: Action): Option[ZonedDateTime] =
      whenMoved(action, isMoveToDoingList)

    def whenMovedToDoneList(action: Action): Option[ZonedDateTime] =
      whenMoved(action, isMoveToDoneList)
  }
}
