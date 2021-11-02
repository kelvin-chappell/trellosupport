package trellosupport

import zio.{Has, ZIO}

case class Config(boardId: String, key: String, token: String)

object Config {

  private def env(name: String) = {
    zio.System.env(name).someOrFail(new RuntimeException(s"Environment missing value '$name'"))
  }

  val load: ZIO[Has[zio.System], Throwable, Config] =
    for {
      key <- env("key")
      token <- env("token")
    } yield Config(boardId = "3cFFCcj4", key, token)
}
