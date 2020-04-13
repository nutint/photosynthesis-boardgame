package com.nat.typeprogramming.service.model

import java.time.{Duration, Instant}
import java.util.Date

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.TableDrivenPropertyChecks._

class Example2Spec extends FreeSpec with Matchers {

  /**
   * REQ-03
   * User Story
   * As a Business Owner
   * I want NonVerified account which has been inactive for 1 year to be Inactive
   * So I can save the operation cost
   */

  "AttributeExample" - {

    import Example2.AttributeExample._

    "verify" - {

      val expireDate = new Date()
      val lastActiveAt = new Date()

      "should fail with reason when try to verify verified account" in {
        verify(
          Account("John", "john@email.com", Verified, expireDate, lastActiveAt)
        ) shouldBe Left("Already verified")
      }

      "should fail with reason when try to verify premium account" in {
        verify(
          Account("John", "john@email.com", Premium, expireDate, lastActiveAt)
        ) shouldBe Left("Already verified")
      }

      "should become verified account when verify verified account" in {
        verify(
          Account("John", "john@email.com", NonVerified, expireDate, lastActiveAt)
        ) shouldBe Right(Account("John", "john@email.com", Verified, expireDate, lastActiveAt))
      }
    }

    "sendReminderEmail" - {

      val lastActiveAt = new Date()
      val inRangedPremiumExpireDate = Date.from(Instant.now().plus(Duration.ofDays(6)))
      val notInRangedPremiumExpireDate = Date.from(Instant.now().plus(Duration.ofDays(8)))
      val noOfDaysBeforeExpire = 7

      "should fail with reason when sending to non-verified account" in {
        sendReminderEmail(
          Account("John", "john@email.com", NonVerified, inRangedPremiumExpireDate, lastActiveAt),
          noOfDaysBeforeExpire
        ) shouldBe Left("Unable to send to Non-Premium account")
      }

      "should fail with reason when sending to verified account" in {
        sendReminderEmail(
          Account("John", "john@email.com", Verified, inRangedPremiumExpireDate, lastActiveAt),
          noOfDaysBeforeExpire
        ) shouldBe Left("Unable to send to Non-Premium account")
      }

      "should fail with reason when sending to premium account but expire date is over 7 day from now" in {
        sendReminderEmail(
          Account("John", "john@email.com", Premium, notInRangedPremiumExpireDate, lastActiveAt),
          noOfDaysBeforeExpire
        ) shouldBe Left(s"Unable to send to Premium account that expire more than $noOfDaysBeforeExpire days from now")
      }

      "should success when premium account and expire date is in 7 day" in {
        sendReminderEmail(
          Account("John", "john@email.com", Premium, inRangedPremiumExpireDate, lastActiveAt),
          noOfDaysBeforeExpire
        ) shouldBe Right(())
      }
    }

    "deactivateAccount" - {

      val premiumExpiredDate = new Date()
      val lastActivityAt = new Date()
      val lastActivityMoreThanOneYear = Date.from(Instant.now().minus(Duration.ofDays(399)))
      val lastActivityLessThan1Year = Date.from(Instant.now().minus(Duration.ofDays(300)))
      val oneYearBeforeNow = Date.from(Instant.now().minus(Duration.ofDays(365)))

      "should fail if the account is verified account" in {

        val examples =
          Table(
            "accountStatus",
            Verified,
            Premium
          )
        forAll(examples) { accountStatus =>
          deactivateAccount(
            Account("John", "john@example.com", accountStatus, premiumExpiredDate, lastActivityAt),
            oneYearBeforeNow
          ) shouldBe Left("Unable to deactivate verified account")
        }
      }

      "should fail if the account is non-verified but lastActive not over 1 year" in {
        deactivateAccount(
          Account("John", "john@example.com", NonVerified, premiumExpiredDate, lastActivityLessThan1Year),
          oneYearBeforeNow
        ) shouldBe Left("Too early to deactivate the account")
      }

      "should success if the account is non-verified and lastActive was over 1 year" in {
        deactivateAccount(
          Account("John", "john@example.com", NonVerified, premiumExpiredDate, lastActivityMoreThanOneYear),
          oneYearBeforeNow
        ) shouldBe Right(())
      }
    }
  }

  "TypeExample" - {

    import Example2.TypeExample._

    "verify" - {

      "should fail with reason when try to verify verified account" in {
        verify(
          VerifiedAccount("John", "john@email.com")
        ) shouldBe Left("Already verified")
      }

      "should fail with reason when try to verify premium account" in {
        verify(
          PremiumAccount("John", "john@email.com", new Date())
        ) shouldBe Left("Already verified")
      }

      "should become verified account when verify verified account" in {
        verify(
          NonVerifiedAccount("John", "john@email.com")
        ) shouldBe Right(VerifiedAccount("John", "john@email.com"))
      }
    }

    "sendReminderEmail" - {

      val inRangedPremiumExpireDate = Date.from(Instant.now().plus(Duration.ofDays(6)))
      val notInRangedPremiumExpireDate = Date.from(Instant.now().plus(Duration.ofDays(8)))
      val noOfDaysBeforeExpire = 7

      "should fail with reason when sending to premium account but expire date is over 7 day from now" in {
        sendReminderEmail(
          PremiumAccount("John", "john@email.com", notInRangedPremiumExpireDate),
          noOfDaysBeforeExpire
        ) shouldBe Left(s"Unable to send to Premium account that expire more than $noOfDaysBeforeExpire days from now")
      }

      "should success when premium account and expire date is in 7 day" in {
        sendReminderEmail(
          PremiumAccount("John", "john@email.com", inRangedPremiumExpireDate),
          noOfDaysBeforeExpire
        ) shouldBe Right(())
      }
    }
  }
}
