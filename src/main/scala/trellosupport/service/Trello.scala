package trellosupport.service

import scalaj.http.Http
import trellosupport.{Config, Failure}
import trellosupport.model.Trello._
import upickle.default.read
import zio._

trait Trello {
  def allLists: IO[Failure, Seq[List]]
  def allCards: IO[Failure, Seq[Card]]
  def cardMovementHistory(card: Card): IO[Failure, Seq[Action]]
}

object Trello {
  val allLists: ZIO[Has[Trello], Failure, Seq[List]] = ZIO.serviceWith(_.allLists)

  val allCards: ZIO[Has[Trello], Failure, Seq[Card]] = ZIO.serviceWith(_.allCards)

  def cardMovementHistory(card: Card): ZIO[Has[Trello], Failure, Seq[Action]] =
    ZIO.serviceWith(_.cardMovementHistory(card))
}

object TrelloLive {

  private def fromConfig(config: Config) = {
    new Trello {

      def allLists: IO[Failure, Seq[List]] =
        for {
          response <- IO
            .attempt(
              Http(s"https://api.trello.com/1/boards/${config.boardId}/lists")
                .param("key", config.key)
                .param("token", config.token)
                .asString
            )
            .mapError(Failure.fromThrowable)
          lists <- IO.attempt(read[Seq[List]](response.body)).mapError(Failure.fromThrowable)
        } yield lists

      def allCards: IO[Failure, Seq[Card]] =
        for {
          response <- IO
            .attempt(
              Http(s"https://api.trello.com/1/boards/${config.boardId}/cards/all")
                .param("key", config.key)
                .param("token", config.token)
                .asString
            )
            .mapError(Failure.fromThrowable)
          cards <- IO.attempt(read[Seq[Card]](response.body)).mapError(Failure.fromThrowable)
        } yield cards

      def cardMovementHistory(card: Card): IO[Failure, Seq[Action]] =
        for {
          response <- IO
            .attempt(
              Http(s"https://api.trello.com/1/cards/${card.id}/actions")
                .param("key", config.key)
                .param("token", config.token)
                .asString
            )
            .mapError(Failure.fromThrowable)
          actions <- IO
            .attempt(
              read[Seq[Action]](response.body).filter(_.`type` == "updateCard")
            )
            .mapError(Failure.fromThrowable)
        } yield actions
    }
  }

  val layer: RLayer[Has[Config], Has[Trello]] = RIO.service[Config].map(fromConfig).toLayer
}
