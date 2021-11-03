package trellosupport.model

import trellosupport.model.Trello.Card
import trellosupport.model.Trello.Card.{isP1, isP2, isP3}

import java.time.Duration

case class ReportRow(
    week: Week,
    p1: ReportSubRow,
    p2: ReportSubRow,
    p3: ReportSubRow,
    totalTimeTaken: Duration
)

object ReportRow {

  def fromCards(cards: Seq[CompletedCard]): Seq[ReportRow] =
    CompletedCard.groupedByWeek(cards).map { case (week, cards) => reportForWeek(week, cards) }

  private def reportForWeek(week: Week, cards: Seq[CompletedCard]) = {
    def subRow(p: Card => Boolean) = ReportSubRow.fromCards(cards.filter(x => p(x.card)))
    val p1 = subRow(isP1)
    val p2 = subRow(isP2)
    val p3 = subRow(isP3)
    ReportRow(
      week,
      p1,
      p2,
      p3,
      totalTimeTaken = p1.timeTaken.plus(p2.timeTaken).plus(p3.timeTaken)
    )
  }
}

case class ReportSubRow(count: Int, timeTaken: Duration)

object ReportSubRow {

  private def timeTaken(card: CompletedCard) = Duration.between(card.timeStarted, card.timeDone)

  def fromCards(cards: Seq[CompletedCard]): ReportSubRow =
    ReportSubRow(
      count = cards.length,
      timeTaken = cards.map(timeTaken).foldLeft(Duration.ZERO)((total, curr) => total.plus(curr))
    )
}
