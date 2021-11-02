package trellosupport.model

import trellosupport.model.CardWithWeekDone.groupedByWeek
import trellosupport.model.Trello.Card.{isP1, isP2, isP3}

import java.time.LocalDate

case class ReportRow(week: Week, p1Count: Int, p2Count: Int, p3Count: Int)

object ReportRow {

  private val defaultWeek: Week = Week(LocalDate.of(1970, 1, 1))

  def fromCards(cards: Seq[CardWithWeekDone]): Seq[ReportRow] =
    groupedByWeek(cards).map {
      case (None, cards)       => reportForWeek(week = defaultWeek, cards)
      case (Some(week), cards) => reportForWeek(week, cards)
    }

  private def reportForWeek(week: Week, cards: Seq[CardWithWeekDone]) =
    ReportRow(
      week,
      p1Count = cards.count(x => isP1(x.card)),
      p2Count = cards.count(x => isP2(x.card)),
      p3Count = cards.count(x => isP3(x.card))
    )
}
