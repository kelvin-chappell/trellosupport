package trellosupport.model

import trellosupport.model.Trello.Action

import java.time.DayOfWeek.MONDAY
import java.time.{LocalDate, ZonedDateTime}
import java.time.temporal.TemporalAdjusters.previous

case class Week(beginning: LocalDate)

object Week {

  private def forDate(date: LocalDate): Week = {
    if (date.getDayOfWeek == MONDAY) Week(date)
    else Week(date.`with`(previous(MONDAY)))
  }

  def forTime(time: ZonedDateTime): Week = {
    if (time.getDayOfWeek == MONDAY) Week(time.toLocalDate)
    else Week(time.toLocalDate.`with`(previous(MONDAY)))
  }

  private def done(history: Seq[Action]): Option[Week] =
    history.find(Action.isMoveToDoneList).map(action => Week.forDate(action.date.toLocalDate))
}
