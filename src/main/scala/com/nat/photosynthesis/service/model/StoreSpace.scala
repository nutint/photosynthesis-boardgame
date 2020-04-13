package com.nat.photosynthesis.service.model

import scala.util.Try

case class StoreSpace[A<:Plant](prices: List[Int], currItem: Int) {
  def currentPrice: Either[String, Int] = Either.cond(Try(prices(currItem)).isSuccess, prices(currItem), "Out of stock")
  def take: Either[String, StoreSpace[A]] =
    if(currItem == prices.length) {
      Left("Item not available")
    } else {
      Right(copy(currItem = currItem + 1))
    }

  def putBack: Either[String, StoreSpace[A]] =
    if(currItem == 0) {
      Left("Already full")
    } else {
      Right(copy(
        currItem = currItem - 1
      ))
    }
}

object StoreSpace {
  def apply[A<:Plant](prices: List[Int]): StoreSpace[A] = StoreSpace[A](prices, 0)
  def apply[A<:Plant](prices: Int*): StoreSpace[A] = apply(prices.toList)
}

