package com.nat.typeprogramming.service.model

import java.time.{Duration, Instant}
import java.util.Date

object Example2 {

  object AttributeExample {

    sealed trait AccountStatus
    case object NonVerified extends AccountStatus
    case object Verified extends AccountStatus
    case object Premium extends AccountStatus

    case class Account(
      name: String,
      email: String,
      accountStatus: AccountStatus,
      premiumExpireDate: Date,
      lastActivityAt: Date
    )

    def verify(account: Account): Either[String, Account] = account.accountStatus match {
      case NonVerified => Right(account.copy(accountStatus = Verified))
      case Verified | Premium => Left("Already verified")
    }

    def sendReminderEmail(account: Account, noOfDaysBeforeExpire: Int): Either[String, Unit] = {
      val nDaysFromNow = Date.from(Instant.now().plus(Duration.ofDays(noOfDaysBeforeExpire)))
      val isInReminderPeriod = nDaysFromNow.after(account.premiumExpireDate)
      (account.accountStatus, isInReminderPeriod) match {
        case (Premium, true) => Right(())
        case (Premium, false) => Left(s"Unable to send to Premium account that expire more than $noOfDaysBeforeExpire days from now")
        case _ => Left("Unable to send to Non-Premium account")
      }
    }

    def deactivateAccount(account: Account, date: Date): Either[String, Unit] = {
      val noActivityMoreThan1Year = account.lastActivityAt.before(date)
      (account.accountStatus, noActivityMoreThan1Year) match {
        case (NonVerified, true) => Right(())
        case (NonVerified, false) => Left(s"Too early to deactivate the account")
        case _ => Left("Unable to deactivate verified account")
      }
    }
  }

  object TypeExample {

    sealed trait Account
    case class NonVerifiedAccount(name: String, email: String) extends Account {
      def verify: VerifiedAccount = VerifiedAccount(name, email)
    }
    case class VerifiedAccount(name: String, email: String) extends Account
    case class PremiumAccount(name: String, email: String, expireDate: Date) extends Account

    def verify(account: Account): Either[String, Account] = account match {
      case n: NonVerifiedAccount => Right(n.verify)
      case _ => Left("Already verified")
    }

    def sendReminderEmail(premiumAccount: PremiumAccount, noOfDaysBeforeExpire: Int): Either[String, Unit] = {
      val nDaysFromNow = Date.from(Instant.now().plus(Duration.ofDays(noOfDaysBeforeExpire)))
      val isInReminderPeriod = nDaysFromNow.after(premiumAccount.expireDate)
      if(isInReminderPeriod) Right(())
      else Left(s"Unable to send to Premium account that expire more than $noOfDaysBeforeExpire days from now")
    }
  }
}