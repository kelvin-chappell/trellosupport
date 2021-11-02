package trellosupport

import trellosupport.model.Trello.Card.{isP1, isP2, isP3}
import trellosupport.model.Trello._
import trellosupport.model.{CardWithMovement, ReportRow, Week}
import trellosupport.service.{Trello, TrelloLive}
import zio._

object Main extends ZIOAppDefault {

  private def refineWithWeekDone(card: Card): ZIO[Has[Trello], Failure, CardWithMovement] =
    Trello.cardMovementHistory(card).map(CardWithMovement.fromHistory(card))

  private val program: ZIO[Has[Trello] with Has[Console], Failure, Unit] =
    for {
      allCards <- Trello.allCards
      cards <- ZIO
        .foreachPar(
          allCards.filter(card => isP1(card) || isP2(card) || isP3(card)).slice(25, 26)
        )(
          refineWithWeekDone
        )
        .debug("refined")
      _ <- Console.printLine("Week,P1Count,P2Count,P3Count").mapError(Failure.fromThrowable)
      _ <- ZIO
        .foreachDiscard(ReportRow.fromCards(cards)) { row =>
          Console.printLine(s"${row.week.beginning},${row.p1Count},${row.p2Count},${row.p3Count}")
        }
        .mapError(Failure.fromThrowable)
    } yield ()

  def run: ZIO[zio.ZEnv with Has[ZIOAppArgs], Any, Any] =
    program.injectCustom(Config.load.toLayer, TrelloLive.layer)
}
