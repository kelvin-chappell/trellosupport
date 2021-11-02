package trellosupport.model

import trellosupport.model.Trello.Action.{whenMovedToDoing, whenMovedToDone}
import trellosupport.model.Trello.{Action, Card}

import java.time.ZonedDateTime

case class CardWithMovement(
    card: Card,
    weekDone: Option[Week],
    timeStarted: Option[ZonedDateTime],
    timeDone: Option[ZonedDateTime]
)

object CardWithMovement {

  def fromHistory(card: Card)(history: Seq[Action]): CardWithMovement =
    CardWithMovement(
      card,
      weekDone = Week.done(history),
      timeStarted = history.flatMap(whenMovedToDoing).headOption,
      timeDone = history.flatMap(whenMovedToDone).headOption
    )

  def groupedByWeek(cards: Seq[CardWithMovement]): Seq[(Option[Week], Seq[CardWithMovement])] =
    cards.groupBy(_.weekDone).toSeq.sortBy { case (week, _) => week.map(_.beginning) }
}
