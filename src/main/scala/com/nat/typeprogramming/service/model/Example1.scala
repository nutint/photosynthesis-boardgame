package com.nat.typeprogramming.service.model

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