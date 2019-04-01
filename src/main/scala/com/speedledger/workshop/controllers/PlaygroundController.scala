package com.speedledger.workshop.controllers

import java.time.{Duration, LocalDate}

import cats.effect._
import org.http4s._
import org.http4s.dsl.io._

import scala.util.Try

object PlaygroundController {

  def dateDiff(dateA: LocalDate, dateB: LocalDate) = IO(Duration.between(dateA.atStartOfDay(), dateB.atStartOfDay()).toDays)

  object LocalDateVar {
    def unapply(str: String): Option[LocalDate] = {
      if (!str.isEmpty)
        Try(LocalDate.parse(str)).toOption
      else
        None
    }
  }

  def apply() = HttpService[IO] {
    // task 0: make this route return the string "pong"
    case GET -> Root / "ping" => Ok("pong")

    // task 1: add a route on "calc/{a}/plus/{b}" that returns the sum of a and b as a string
    case GET -> Root / "calc" / IntVar(a) / "plus" / IntVar(b) => Ok((a  + b).toString)

    // task 2: add a route on "/headers/{headerName}" that returns either the value of the header, or the string "header not found"
    case req@GET -> Root / "headers" / str =>
      Ok(req.headers.find(h => h.name.value == str).map(_.value).getOrElse("header not found"))

    // task 3: add a route on "/dateDiff/{dateA}/and/{dateB}" that returns number of days between dates in s"that is $days days"
    case GET -> Root / "dateDiff" / LocalDateVar(a) / "and" / LocalDateVar(b) => Ok(dateDiff(a, b).map(d => s"that is $d days"))

    // this helps us to understand if tests fail
    case req => NotFound(s"could not find ${req.method} ${req.uri.renderString}")
  }
}
