object Domain {

  type Die  = Int
  type Roll = List[Die]

  final val numberOfDice        = 5
  final val smallStraight       = Set(1, 2, 3, 4, 5)
  final val smallStraightPoints = smallStraight.sum
  final val largeStraight       = Set(2, 3, 4, 5, 6)
  final val largeStraightPoints = largeStraight.sum
  final val yahtzeePoints       = 50

  sealed trait Category {
    def roll: Roll
  }

  case class One(roll: Roll)           extends Category
  case class Two(roll: Roll)           extends Category
  case class Three(roll: Roll)         extends Category
  case class Four(roll: Roll)          extends Category
  case class Five(roll: Roll)          extends Category
  case class Six(roll: Roll)           extends Category
  case class Pair(roll: Roll)          extends Category
  case class TwoPairs(roll: Roll)      extends Category
  case class Tris(roll: Roll)          extends Category
  case class Poker(roll: Roll)         extends Category
  case class SmallStraight(roll: Roll) extends Category
  case class LargeStraight(roll: Roll) extends Category
  case class FullHouse(roll: Roll)     extends Category
  case class Yahtzee(roll: Roll)       extends Category
  case class Chance(roll: Roll)        extends Category

  def points(category: Category): Int = {
    def groupsSum(roll: Roll, c: Int): Iterable[Int] =
      roll.groupBy(identity).mapValues(_.size).collect { case (die, count) if count >= c => die * c }

    def sumSameEl(roll: Roll, die: Die): Int = roll.filter(_ == die).sum

    category match {
      case One(roll)           => sumSameEl(roll, 1)
      case Two(roll)           => sumSameEl(roll, 2)
      case Three(roll)         => sumSameEl(roll, 3)
      case Four(roll)          => sumSameEl(roll, 4)
      case Five(roll)          => sumSameEl(roll, 5)
      case Six(roll)           => sumSameEl(roll, 6)
      case Pair(roll)          => groupsSum(roll, 2) match { case Nil => 0; case _ @list => list.max }
      case TwoPairs(roll)      => groupsSum(roll, 2) match { case p1 :: p2 :: Nil => p1 + p2; case _ => 0 }
      case Tris(roll)          => groupsSum(roll, 3) match { case t :: Nil => t; case _ => 0 }
      case Poker(roll)         => groupsSum(roll, 4) match { case t :: Nil => t; case _ => 0 }
      case SmallStraight(roll) => if (roll.toSet == smallStraight) smallStraightPoints else 0
      case LargeStraight(roll) => if (roll.toSet == largeStraight) largeStraightPoints else 0
      case Yahtzee(roll)       => if (groupsSum(roll, 5).nonEmpty) yahtzeePoints else 0
      case Chance(roll)        => roll.sum
      case FullHouse(roll) =>
        roll.groupBy(identity).mapValues(_.size).filter { case (_, count) => count >= 2 }.toList match {
          case (e1, c1) :: (e2, c2) :: Nil if c1 == 3 || c2 == 3 => e1 * c1 + e2 * c2
          case _                                                 => 0
        }
    }
  }

  def fromString(category: String)(roll: Roll): Option[Category] = category match {
    case "One"           => Some(One(roll))
    case "Two"           => Some(Two(roll))
    case "Three"         => Some(Three(roll))
    case "Four"          => Some(Four(roll))
    case "Five"          => Some(Five(roll))
    case "Six"           => Some(Six(roll))
    case "Pair"          => Some(Pair(roll))
    case "TwoPairs"      => Some(TwoPairs(roll))
    case "Tris"          => Some(Tris(roll))
    case "Poker"         => Some(Poker(roll))
    case "SmallStraight" => Some(SmallStraight(roll))
    case "LargeStraight" => Some(LargeStraight(roll))
    case "Yahtzee"       => Some(Yahtzee(roll))
    case "Chance"        => Some(Chance(roll))
    case "FullHouse"     => Some(FullHouse(roll))
    case _               => None
  }

}
