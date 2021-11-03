package trellosupport.model

import trellosupport.model.Trello.Action.{whenMovedToDoingList, whenMovedToDoneList}
import trellosupport.model.Trello.Card.whenCreated
import trellosupport.model.Trello.{Action, Card, List}

import java.time.ZonedDateTime

case class CompletedCard(
    card: Card,
    timeStarted: ZonedDateTime,
    timeDone: ZonedDateTime
)

object CompletedCard {

  private def whenCreatedInDoingList(
      card: Card,
      history: Seq[Action],
      doingList: List
  ): Option[ZonedDateTime] =
    history.headOption match {
      case None if card.idList == doingList.id => Some(whenCreated(card))
      case Some(action) if action.data.listBefore.exists(_.id == doingList.id) =>
        Some(whenCreated(card))
      case _ => None
    }

  def fromHistory(doingList: List, card: Card)(history: Seq[Action]): Option[CompletedCard] =
    for {
      timeStarted <- history
        .flatMap(whenMovedToDoingList)
        .headOption
        .orElse(whenCreatedInDoingList(card, history, doingList))
      timeDone <- history.flatMap(whenMovedToDoneList).headOption
    } yield CompletedCard(card, timeStarted, timeDone)

  def groupedByWeek(cards: Seq[CompletedCard]): Seq[(Week, Seq[CompletedCard])] =
    cards.groupBy(card => Week.forTime(card.timeDone)).toSeq.sortBy { case (week, _) =>
      week.beginning
    }
}
