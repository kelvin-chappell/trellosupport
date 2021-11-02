package trellosupport

case class Failure(message: String)

object Failure {
  def fromThrowable(throwable: Throwable): Failure = Failure(throwable.getMessage)
}
