package com.github.fedragon.dominion

import scalaz.Scalaz._

sealed trait Card {
  val name: String
  val cost: Coins

  override def toString = name
}

case class Coins(value: Int) extends AnyVal {
  def +(that: Coins) = Coins(value + that.value)
  def -(that: Coins) = Coins(value - that.value)
}

case object Curse extends Card {
  override val name = "Curse"
  override val cost = Coins(0)
  val value: Int = -1

  def unapply(c: Card): Option[Curse.type] = c match {
    case cu: Curse.type => cu.some
    case _ => None
  }
}


sealed trait Modifiers

trait Attack extends Modifiers

trait Kingdom extends Modifiers

trait Reaction extends Modifiers

abstract class Action(val name: String, val cost: Coins) extends Card

object Action {
  def unapply(c: Card): Option[Action] = c match {
    case a: Action => a.some
    case _ => None
  }
}

abstract class Treasure(val name: String, val cost: Coins, val value: Coins) extends Card

object Treasure {
  def unapply(c: Card): Option[Treasure] = c match {
    case t: Treasure => t.some
    case _ => None
  }
}

sealed trait CardValue

case class FixedValue(value: Int) extends CardValue {
  def apply(): Int = value
}

case class FunctionValue(value: Int => Int) extends CardValue {
  def apply(n: Int): Int = value(n)
}

abstract class Victory(val name: String, val cost: Coins, val value: CardValue) extends Card {
  def value(nOfCards: Int): Int = value match {
    case v: FixedValue => v()
    case f: FunctionValue => f(nOfCards)
  }
}

object Victory {
  def unapply(c: Card): Option[Victory] = c match {
    case v: Victory => v.some
    case _ => None
  }
}

