package com.github.fedragon.dominion

sealed trait Card {
  val name: String
  val cost: Coins

  override def toString = name
}

case class Coins(value: Int) extends AnyVal {
  def +(that: Coins) = Coins(value + that.value)
  def -(that: Coins) = Coins(value - that.value)
}

sealed trait Modifiers

trait Attack extends Modifiers

trait Reaction extends Modifiers

abstract class Action(val name: String, val cost: Coins) extends Card

object Action {
  def unapply(c: Card): Option[Action] = c match {
    case a: Action => Some(a)
    case _ => None
  }
}

abstract class Treasure(val name: String, val cost: Coins, val value: Coins) extends Card

object Treasure {
  def unapply(c: Card): Option[Treasure] = c match {
    case t: Treasure => Some(t)
    case _ => None
  }
}

case class CardValue(value: Int) extends AnyVal

abstract class Victory(val name: String, val cost: Coins, val value: CardValue) extends Card

