package example

object Hello extends Greeting with App {
  List(1, 2, 3)
    .map(greetUser(_))
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
}

case class User(id: Long, name: Option[String])

