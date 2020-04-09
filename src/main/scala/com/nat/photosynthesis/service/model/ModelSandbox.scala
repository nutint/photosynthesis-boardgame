package com.nat.photosynthesis.service.model

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

      case class Account(name: String, email: String, accountStatus: AccountStatus) {
        def verify: Account = accountStatus match {
          case NonVerified => copy(accountStatus = Verified)
          case _ => this
        }
      }

      // Usage
      val account = Account("John", "john@gmail.com", NonVerified)
      val mayBeVerifiedAccount = account.verify

    }

    object TypeExample {
      sealed trait UserAccount
      case class NonVerifiedAccount(name: String, email: String) extends UserAccount {
        def verify: VerifiedAccount = VerifiedAccount(name, email)
      }
      case class VerifiedAccount(name: String, email: String) extends UserAccount
      case class PremiumAccount(name: String, email: String) extends UserAccount

      // REQ-01
      // Usage
      val nonVerifiedAccount = NonVerifiedAccount("John", "john@gmail.com")
      val verifiedAccount = nonVerifiedAccount.verify

      // verifiedAccount
      val maryVerifiedAccount = VerifiedAccount("Mary", "mary@gmail.com")
      // maryVerifiedAccount.verify // compile error


    }
  }


}
