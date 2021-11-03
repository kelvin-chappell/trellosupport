package trellosupport

import trellosupport.model.Trello.Card.{isP1, isP2, isP3}
import trellosupport.model.Trello._
import trellosupport.model.{CompletedCard, ReportRow}
import trellosupport.service.{Trello, TrelloLive}
import zio._

object Main extends ZIOAppDefault {

  private def refineWithWeekDone(doingList: List)(
      card: Card
  ): ZIO[Has[Trello], Failure, Option[CompletedCard]] =
    Trello.cardMovementHistory(card).map(CompletedCard.fromHistory(doingList, card))

  private val program: ZIO[Has[Trello] with Has[Console], Failure, Unit] =
    for {
      maybeDoingList <- Trello.allLists.map(_.find(_.name == "Doing"))
      doingList <- ZIO.fromOption(maybeDoingList).orElseFail(Failure("No Doing list"))
      allCards <- Trello.allCards
      cards <- ZIO
        .foreachPar(allCards.filter(card => isP1(card) || isP2(card) || isP3(card)))(
          refineWithWeekDone(doingList)
        )
        .map(_.flatten)
        .debug("refined")
      _ <- Console
        .printLine("Week,P1Count,P1Duration,P2Count,P2Duration,P3Count,P3Duration,TotDuration")
        .mapError(Failure.fromThrowable)
      _ <- ZIO
        .foreachDiscard(ReportRow.fromCards(cards)) { row =>
          Console.printLine(
            s"${row.week.beginning},${row.p1.count},${row.p1.timeTaken.toHours},${row.p2.count},${row.p2.timeTaken.toHours},${row.p3.count},${row.p3.timeTaken.toHours},${row.totalTimeTaken.toHours}"
          )
        }
        .mapError(Failure.fromThrowable)
    } yield ()

  def run: ZIO[zio.ZEnv with Has[ZIOAppArgs], Any, Any] =
    program.injectCustom(Config.load.toLayer, TrelloLive.layer)
}
