package trellosupport.model

import trellosupport.model.Trello.Card

case class CardWithWeekDone(card: Card, weekDone: Option[Week])

object CardWithWeekDone {

  def groupedByWeek(cards: Seq[CardWithWeekDone]): Seq[(Option[Week], Seq[CardWithWeekDone])] =
    cards.groupBy(_.weekDone).toSeq.sortBy { case (week, _) => week.map(_.beginning) }
}
