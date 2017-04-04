sealed abstract class Suit(val value: Int)
case object Club    extends Suit(1)
case object Diamond extends Suit(2)
case object Heart   extends Suit(3)
case object Spade   extends Suit(4)

sealed abstract class Number(val value: Int)
case object Two   extends Number(1)
case object Three extends Number(2)
case object Four  extends Number(3)
case object Five  extends Number(4)
case object Six   extends Number(5)
case object Seven extends Number(6)
case object Eight extends Number(7)
case object Nine  extends Number(8)
case object Ten   extends Number(9)
case object Jack  extends Number(10)
case object Qeen  extends Number(11)
case object King  extends Number(12)
case object Ace   extends Number(13)

sealed abstract class PokerHand(val value: Int)
case object HightCard     extends PokerHand(1)
case object OnePair       extends PokerHand(2)
case object TwoPair       extends PokerHand(3)
case object ThreeCard     extends PokerHand(4)
case object Straight      extends PokerHand(5)
case object Flush         extends PokerHand(6)
case object FullHouse     extends PokerHand(7)
case object FourCard      extends PokerHand(8)
case object StraightFlush extends PokerHand(9)

case class Card(suit: Suit, number: Number) {
 def value: Int = this.suit.value + this.number.value
}

type Cards = List[Card]

case class GameMaster(cards: Cards) {
  private
  def rspan[T](cards: List[T]): List[List[T]] = {
    cards.span(cards.headOption.getOrElse(Nil) == _) match {
      case (h, Nil) => h :: Nil
      case (h, t)   => h :: rspan(t)
    }
  }

  private
  def cardValueSort(f: Card => Int): List[Int] = rspan(cards.map(f).sorted).map(_.length)

  private
  def isStairs(cards: Cards): Boolean =
    this.cards.map(_.number.value).sliding(2).forall(x => x(1) - x(0) == 1)

  def decision: PokerHand = (cardValueSort((x: Card) => x.suit.value)
                            ,cardValueSort((x: Card) => x.number.value)) match {
    case x@(List(5), List(1,1,1,1,1)) if (isStairs(cards)) => StraightFlush
    case (_, List(1,4))                                    => FourCard
    case (_, List(2,3))                                    => FullHouse
    case (List(5), _)                                      => Flush
    case x@(_, List(1,1,1,1,1)) if (isStairs(cards))       => Straight
    case (_, List(1,1,3))                                  => ThreeCard
    case (_, List(1,2,2))                                  => TwoPair
    case (_, List(1,1,1,2))                                => OnePair
    case _                                                 => HightCard
  }
}
