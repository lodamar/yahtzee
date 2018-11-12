import java.io.IOException
import java.util.concurrent.ThreadLocalRandom

import Domain._
import scalaz.zio.IO.parAll
import scalaz.zio._
import scalaz.zio.console._

object Yahtzee extends App {
  def run(args: List[String]): IO[Nothing, ExitStatus] =
    game.redeemPure(
      _ => ExitStatus.ExitNow(1),
      _ => ExitStatus.ExitNow(0)
    )

  def rollDice: IO[Nothing, Roll] = parAll(List.fill(numberOfDice)(rollDie))
  def rollDie: IO[Nothing, Die]   = IO.sync(ThreadLocalRandom.current().nextInt(6) + 1)

  def reRolls(roll: Roll, indexes: Set[Int]): IO[Nothing, Roll] =
    if (indexes.isEmpty) IO.now(roll)
    else
      for {
        die   <- rollDie
        rolls <- reRolls(roll.updated(indexes.head - 1, die), indexes.tail)
      } yield rolls

  def valid(dice: String): Option[Set[Int]] = {
    val choices = dice.map(_.asDigit).toSet
    if (dice.length <= numberOfDice && choices.count(c => c <= numberOfDice && c >= 1) == dice.length) Some(choices)
    else None
  }

  def untilValid(dice: String): IO[IOException, Set[Int]] = valid(dice).map(IO.now).getOrElse(askAgain)

  def askAgain: IO[IOException, Set[Int]] =
    for {
      newChoices <- putStrLn("Invalid dice choices, please choose again") *> getStrLn
      ids        <- untilValid(newChoices)
    } yield ids

  def reRollsCycle(roll: Roll, leftAttempts: Int): IO[IOException, Roll] =
    for {
      dice    <- putStrLn("Choose dice to rerolls") *> getStrLn
      choices <- untilValid(dice)
      newRoll <- reRolls(roll, choices)
      _       <- putStrLn(s"Your new roll is ${newRoll.mkString(" ")}")
      last <- if (leftAttempts > 0)
               choice("Would you like to reroll again?", reRollsCycle(newRoll, leftAttempts - 1), IO.unit(newRoll))
             else IO.now(newRoll)
    } yield last

  def choice[A](question: String, yes: IO[IOException, A], no: IO[IOException, A]): IO[IOException, A] =
    for {
      res <- putStrLn(question) *> getStrLn
      io  <- if (res == "yes") yes else no
    } yield io

  def category(roll: Roll): IO[IOException, Category] =
    for {
      cat <- putStrLn("Choose category for this roll") *> getStrLn
      c   <- fromString(cat)(roll).map(IO.now).getOrElse(putStrLn("Wrong category!") *> category(roll))
    } yield c

  def play(categories: List[Category]): IO[IOException, List[Category]] =
    for {
      _      <- putStrLn("Press any key to roll") *> getStrLn
      roll   <- rollDice
      _      <- putStrLn(s"Your roll is ${roll.mkString(" ")}.\n")
      reroll <- choice("Would you like to reroll?", yes = reRollsCycle(roll, 2), no = IO.unit(roll))
      cat    <- category(reroll)
      _      <- putStrLn(s"Your points for this roll are: ${points(cat)}")
      cats   = cat :: categories
      _      <- if (cats.size <= 15) play(cats) else IO.unit
    } yield cats

  def game: IO[IOException, Unit] =
    for {
      _     <- putStrLn("Welcome to Yahtzee")
      cats  <- play(List())
      total = cats.foldLeft(0)(_ + points(_))
      _     <- putStrLn(s"Your total score is $total")
    } yield ()
}
