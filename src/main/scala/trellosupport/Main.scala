package trellosupport

import scalaj.http.Http
import trellosupport.Trello._
import upickle.default._

import java.time.DayOfWeek.MONDAY
import java.time.LocalDate
import java.time.temporal.TemporalAdjusters.previous
import scala.util.chaining.scalaUtilChainingOps

object Main extends App {

  val key = sys.env("key")
  val token = sys.env("token")
  val boardId = "3cFFCcj4"

  case class DateRange(start: LocalDate, end: LocalDate)

  case class Week(beginning: LocalDate)

  def allCards(): Seq[Card] =
    Http(s"https://api.trello.com/1/boards/$boardId/cards/all")
      .param("key", key)
      .param("token", token)
      .asString
      .body
      .pipe(read[Seq[Card]](_))

  def cardMovementHistory(card: Card): Seq[Action] =
    Http(s"https://api.trello.com/1/cards/${card.id}/actions")
      .param("key", key)
      .param("token", token)
      .asString
      .body
      .pipe(read[Seq[Action]](_, trace = true))
      .filter(_.`type` == "updateCard")

  def p1s(card: Card): Boolean = card.labels.exists(_.name == "P1")
  def p2s(card: Card): Boolean = card.labels.exists(_.name == "P2")
  def p3s(card: Card): Boolean = card.labels.exists(_.name == "P3")

  def doneAction(actions: Seq[Action]): Option[Action] =
    actions.find(_.data.listAfter.exists(_.name.startsWith("Done ")))

  def doneWeek(card: Card, history: Card => Seq[Action]): Option[Week] = {
    def week(date: LocalDate): Week = {
      if (date.getDayOfWeek == MONDAY) Week(date)
      else Week(date.`with`(previous(MONDAY)))
    }
    doneAction(history(card)).map(action => week(action.date.toLocalDate))
  }

  def groupedByDoneWeek(
      cards: Seq[Card],
      history: Card => Seq[Action]
  ): Map[Option[Week], Seq[Card]] =
    cards.groupBy(doneWeek(_, history))

  val cards = allCards()

  def show(p: Card => Boolean): Unit = {
    groupedByDoneWeek(cards.filter(p), cardMovementHistory).toSeq
      .sortBy { case (date, _) => date.toString }
      .foreach { case (date, cards) =>
        println(s"$date: ${cards.length}")
        cards.foreach(card => println(card.name))
        println()
      }
  }

  println("=== P1 ===")
  show(p1s)
  println("=== P2 ===")
  show(p2s)
  println("=== P3 ===")
  show(p3s)
}
