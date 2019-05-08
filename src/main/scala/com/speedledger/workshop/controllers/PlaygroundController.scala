package com.speedledger.workshop.controllers

import java.time.{Duration, LocalDate, Period}

import cats.effect._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.util.CaseInsensitiveString

import scala.util.Try

object LocalDateVar {
  def unapply(str: String): Option[LocalDate] = {
    if (str.isEmpty) {
      None
    } else {
      Try(LocalDate.parse(str)).toOption
    }
  }
}

object PlaygroundController {

  def dateDiff(dateA: LocalDate, dateB: LocalDate) = IO(Duration.between(dateA.atStartOfDay(), dateB.atStartOfDay()).toDays())


  def apply() = HttpService[IO] {
    // task 0: make this route return the string "pong"
    case GET -> Root / "ping" => Ok("pong")

    case GET -> Root / "calc" / IntVar(a) / "plus" / IntVar(b) =>
      Ok((a + b).toString)

    case req@GET -> Root / "headers" / headerName => {
      val h = req.headers.get(CaseInsensitiveString(headerName))
      h match {
        case Some(s) => Ok(s.value)
        case None => Ok("header not found")
      }
    }

    case GET -> Root / "dateDiff" / LocalDateVar(dateA) / "and" / LocalDateVar(dateB) => {
      val days = Period.between(dateA, dateB).getDays
      Ok(s"that is $days days")
    }

    // this helps us to understand if tests fail
    case req => NotFound(s"could not find ${req.method} ${req.uri.renderString}")
  }
}
