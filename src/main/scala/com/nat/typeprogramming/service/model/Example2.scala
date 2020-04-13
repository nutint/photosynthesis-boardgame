package com.nat.typeprogramming.service.model

import java.time.{Duration, Instant}
import java.util.Date

object Example2 {

  object AttributeExample {

    sealed trait AccountStatus
    case object NonVerified extends AccountStatus
    case object Verified extends AccountStatus
    case object Premium extends AccountStatus

    case class Account(name: String, email: String, accountStatus: AccountStatus)

    def verify(account: Account): Either[String, Account] = account.accountStatus match {
      case NonVerified => Right(account.copy(accountStatus = Verified))
      case Verified | Premium => Left("Already verified")
    }
  }

  object TypeExample {

    sealed trait Account
    case class NonVerifiedAccount(name: String, email: String) extends Account
    case class VerifiedAccount(name: String, email: String) extends Account
    case class PremiumAccount(name: String, email: String) extends Account

  }
}