package com.nat.typeprogramming.service.model

import org.scalatest.{FreeSpec, Matchers}

class Example2Spec extends FreeSpec with Matchers {

  /**
   * REQ-01
   * User Story
   * As a NonVerified Account
   * I want to verify account
   * So I can use some extra features for verified account
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
}
