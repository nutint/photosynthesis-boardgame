package com.nat.photosynthesis.controller

import akka.http.scaladsl.server.{Directive, Directive1}
import cats.effect.IO

import scala.util.Try

object CustomDirectives {

  def onIOComplete[T](ioResult: IO[T]): Directive1[Try[T]] =
    Directive { inner => ctx =>
      inner(Tuple1(Try[T](ioResult.unsafeRunSync())))(ctx)
    }

}
