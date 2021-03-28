package example

import cats._
import data._
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object Hello extends Greeting with App {
  List(1, 2, 3)
    .map(greetUserEff(_))
    .foreach {
      case Left(error) => println(error)
      case Right(Some(name)) => println(name)
      case Right(None) => println("John Does?")
    }
}

trait Greeting {
  def getUser(id: Long): Either[String, User] =
    id match {
      case 1 => Right(User(1, Option("Mokele-mbembe")))
      case 2 => Right(User(2, None))
      case _ => Left("Who are you?")
    }

  def greetUser(id: Long): Either[String, Option[String]] =
    for {
      user <- getUser(id)
    } yield for {
      name <- user.name
    } yield s"Hello! $name."

  // Monad Transform
  type StringEither[A] = Either[String, A]

  def greetUserT(id: Long): Either[String, Option[String]] =
    (for {
      user <- OptionT.liftF(getUser(id))
      name <- OptionT.fromOption[StringEither](user.name)
    } yield s"Hello! $name").value

  // Eff
  type _stringEither[R] = StringEither |= R
  type Stack = Fx.fx2[Option, StringEither]

  def greetProgram[R: _stringEither : _option](id: Long): Eff[R, String] =
    for {
      user <- either.fromEither(getUser(id))
      name <- fromOption(user.name)
    } yield s"Hello! $name"

  def greetUserEff(id: Long): Either[String, Option[String]] =
    greetProgram[Stack](id)
      .runOption
      .runEither[String]
      .run
}

case class User(id: Long, name: Option[String])

