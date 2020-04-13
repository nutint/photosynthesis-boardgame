package com.nat.typeprogramming.service.model

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

      "should fail with reason when try to verify verified account" in {
        verify(
          Account("John", "john@email.com", Verified)
        ) shouldBe Left("Already verified")
      }

      "should fail with reason when try to verify premium account" in {
        verify(
          Account("John", "john@email.com", Premium)
        ) shouldBe Left("Already verified")
      }

      "should become verified account when verify verified account" in {
        verify(
          Account("John", "john@email.com", NonVerified)
        ) shouldBe Right(Account("John", "john@email.com", Verified))
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
          PremiumAccount("John", "john@email.com")
        ) shouldBe Left("Already verified")
      }

      "should become verified account when verify verified account" in {
        verify(
          NonVerifiedAccount("John", "john@email.com")
        ) shouldBe Right(VerifiedAccount("John", "john@email.com"))
      }
    }

  }
}
