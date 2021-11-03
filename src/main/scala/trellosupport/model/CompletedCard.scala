package trellosupport.model

import trellosupport.model.Trello.Action.{whenMovedToDoing, whenMovedToDone}
import trellosupport.model.Trello.Card.whenCreated
import trellosupport.model.Trello.{Action, Card}

import java.time.ZonedDateTime

case class CompletedCard(
    card: Card,
    timeStarted: ZonedDateTime,
    timeDone: ZonedDateTime
)

object CompletedCard {

  def fromHistory(card: Card)(history: Seq[Action]): Option[CompletedCard] =
    history.flatMap(whenMovedToDone).headOption.map { timeDone =>
      CompletedCard(
        card,
        timeStarted = history.flatMap(whenMovedToDoing).headOption.getOrElse(whenCreated(card)),
        timeDone
      )
    }

  def groupedByWeek(cards: Seq[CompletedCard]): Seq[(Week, Seq[CompletedCard])] =
    cards.groupBy(card => Week.forTime(card.timeDone)).toSeq.sortBy { case (week, _) =>
      week.beginning
    }
}
