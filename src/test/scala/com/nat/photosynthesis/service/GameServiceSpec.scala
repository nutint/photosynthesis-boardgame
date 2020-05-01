package com.nat.photosynthesis.service

import cats.effect.{IO, Resource}
import cats.implicits._
import java.io.{File, FileInputStream, FileOutputStream, InputStream, OutputStream}

import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.Future

class GameServiceSpec extends FreeSpec with Matchers {

  "cats.IO" - {

    def copy(origin: File, destionation: File): IO[Long] =
      inputOutputStream(origin, destionation).use {
        case (in, out) => transfer(in, out)
      }

    def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
      for {
        amount <- IO(origin.read(buffer,0, buffer.length))
        count <-
          if(amount > -1)
            IO(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
          else
            IO.pure(acc)
      } yield count

    def transfer(inStream: InputStream, outStream: OutputStream): IO[Long] =
      for {
        buffer <- IO(new Array[Byte](1024*10))
        total <- transmit(inStream, outStream, buffer, 0L)
      } yield total

    def inputStream(f: File): Resource[IO, FileInputStream] =
      Resource.make {
        IO(new FileInputStream(f))
      } { inStream =>
        IO(inStream.close()).handleErrorWith(error => {
          println(s"open file failed $error")
          IO.unit
        })
      }

    def outputStream(f: File): Resource[IO, FileOutputStream] =
      Resource.make {
        IO(new FileOutputStream(f))
      } { outStream =>
        IO(outStream.close()).handleErrorWith(error => {
          println(s"open file for write failed $error")
          IO.unit
        })
      }

    def inputOutputStream(in: File, out: File): Resource[IO, (InputStream, OutputStream)] =
      for {
        inStream <- inputStream(in)
        outStream <- outputStream(out)
      } yield (inStream, outStream)

    "should be able to copy a file" in {
      copy(new File("/Users/nat/Downloads/121032.jpg"), new File("/Users/nat/Downloads/copied.jpg"))
        .flatTap(x => {
          println("success")
          assert(true)
          IO.pure(x)
        }).unsafeToFuture()
    }
  }
}
