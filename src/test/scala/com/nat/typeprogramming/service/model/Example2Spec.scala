package com.nat.typeprogramming.service.model

import java.time.{Duration, Instant}
import java.util.Date

import org.scalatest.{FreeSpec, Matchers}

class Example2Spec extends FreeSpec with Matchers {

  /**
   * REQ-02
   * User Story
   * As a Premium member
   * I want to keep remind to renew account 7 days before expiration
   * So I can renew account with out any interruption
   */

  "AttributeExample" - {

    import Example2.AttributeExample._

    "verify" - {

      val expireDate = new Date()
      "should fail with reason when try to verify verified account" in {
        verify(
          Account("John", "john@email.com", Verified, expireDate)
        ) shouldBe Left("Already verified")
      }

      "should fail with reason when try to verify premium account" in {
        verify(
          Account("John", "john@email.com", Premium, expireDate)
        ) shouldBe Left("Already verified")
      }

      "should become verified account when verify verified account" in {
        verify(
          Account("John", "john@email.com", NonVerified, expireDate)
        ) shouldBe Right(Account("John", "john@email.com", Verified, expireDate))
      }
    }

    "sendReminderEmail" - {

      val inRangedPremiumExpireDate = Date.from(Instant.now().plus(Duration.ofDays(6)))
      val notInRangedPremiumExpireDate = Date.from(Instant.now().plus(Duration.ofDays(8)))
      val noOfDaysBeforeExpire = 7
      "should fail with reason when sending to non-verified account" in {
        sendReminderEmail(
          Account("John", "john@email.com", NonVerified, inRangedPremiumExpireDate), noOfDaysBeforeExpire
        ) shouldBe Left("Unable to send to Non-Premium account")
      }

      "should fail with reason when sending to verified account" in {
        sendReminderEmail(
          Account("John", "john@email.com", Verified, inRangedPremiumExpireDate), noOfDaysBeforeExpire
        ) shouldBe Left("Unable to send to Non-Premium account")
      }

      "should fail with reason when sending to premium account but expire date is over 7 day from now" in {
        sendReminderEmail(
          Account("John", "john@email.com", Premium, notInRangedPremiumExpireDate), noOfDaysBeforeExpire
        ) shouldBe Left(s"Unable to send to Premium account that expire more than $noOfDaysBeforeExpire days from now")
      }

      "should success when premium account and expire date is in 7 day" in {
        sendReminderEmail(
          Account("John", "john@email.com", Premium, inRangedPremiumExpireDate), noOfDaysBeforeExpire
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
