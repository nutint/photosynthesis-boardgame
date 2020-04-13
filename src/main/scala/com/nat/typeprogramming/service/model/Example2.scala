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
  }

  object TypeExample {

    sealed trait UserAccount
    case class NonVerifiedAccount(name: String, email: String) extends UserAccount
    case class VerifiedAccount(name: String, email: String) extends UserAccount
    case class PremiumAccount(name: String, email: String, expireDate: Date) extends UserAccount

  }
}