package com.nat.photosynthesis.service.model

import java.time.{Duration, Instant, LocalDate}
import java.util.Date

object ModelSandbox {

  object Example1 {

    object AttributeExample {

      sealed trait Gender
      case object Male extends Gender
      case object Female extends Gender
      case object NonBinary extends Gender

      case class Person(name: String, gender: Gender)
    }

    object TypeExample {

      sealed trait Person {
        def name: String
      }

      case class Male(name: String) extends Person
      case class Female(name: String) extends Person
      case class NonBinary(name: String) extends Person
    }

  }

  object Example2 {

    /**
     * REQ-01
     * User Story
     * As a NonVerified Account
     * I want to verify account
     * So I can use features that use verify
     */

    /**
     * REQ-02
     * User Story
     * As a Premium member
     * I want to keep remind to renew account 7 days before expiration
     * So I can renew account with out any interruption
     */

    /**
     * REQ-03
     * User Story
     * As a Business Owner
     * I want NonVerified account which inactive for 1 year to be Inactive
     * So I can save the storage cost
     */

    object AttributeExample {
      sealed trait AccountStatus
      case object NonVerified extends AccountStatus
      case object Verified extends AccountStatus
      case object Premium extends AccountStatus

      case class Account(name: String, email: String, accountStatus: AccountStatus, premiumExpireDate: Date) {
        def verify: Account = accountStatus match {
          case NonVerified => copy(accountStatus = Verified)
          case _ => this
        }
      }

      // REQ-01
      // Usage
      val account = Account("John", "john@gmail.com", NonVerified)
      val mayBeVerifiedAccount = account.verify

      // REQ-02
      def sendRenewalNotification(allAccounts: List[Account], notificationDate: Date) = {
        allAccounts
          .filter(account => account.accountStatus == Premium && account.premiumExpireDate.after(notificationDate))
          .foreach(account => println(s"send notification to account $account"))
      }
      // Usage
      val notificationDate = Date.from(Instant.now().minus(Duration.ofDays(7)))
      val accounts = List(/* */)
      sendRenewalNotification(accounts, notificationDate)
    }

    object TypeExample {
      sealed trait UserAccount
      case class NonVerifiedAccount(name: String, email: String) extends UserAccount {
        def verify: VerifiedAccount = VerifiedAccount(name, email)
      }
      case class VerifiedAccount(name: String, email: String) extends UserAccount
      case class PremiumAccount(name: String, email: String, expireDate: Date) extends UserAccount

      // REQ-01
      // Usage
      val nonVerifiedAccount = NonVerifiedAccount("John", "john@gmail.com")
      val verifiedAccount = nonVerifiedAccount.verify

      // verifiedAccount
      val maryVerifiedAccount = VerifiedAccount("Mary", "mary@gmail.com")
      // maryVerifiedAccount.verify // compile error

      // REQ-02
      def sendRenewalNotification(premiumAccounts: List[PremiumAccount]) =
        premiumAccounts
          .foreach(premiumAccount => println(s"send renewal notification to ${premiumAccount.name} expireAt ${premiumAccount.expireDate}"))

      def sendRenewalNotificationUnsafe(userAccounts: List[UserAccount]) =
        userAccounts
            .flatMap {
              case pm: PremiumAccount => List(pm)
              case _ => Nil
            }
          .foreach(premiumAccount => println(s"send renewal notification to ${premiumAccount.name} expireAt ${premiumAccount.expireDate}"))
    }
  }


}
