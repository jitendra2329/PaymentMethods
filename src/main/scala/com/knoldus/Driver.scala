package com.knoldus

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Driver extends App {
  private val payment = new PaymentServices
  println("Select Option\n1. Make payment\n2. Add payment method\n3. See Account Balance")

  val choice = Try(StdIn.readInt)

  choice match {
    case Success(value) => value match {
      case 1 =>
        if (payment.paymentMethods.isEmpty) { // If you don't have any method added for the transaction
          println("Add payment method first!!")
          payment.addPaymentMethod()
        } else payment.selectPaymentMethod()
      case 2 => payment.addPaymentMethod()
      case 3 => println(payment.seeBalance())
      case _ => println("Invalid Selection!!")
    }

    case Failure(exception) => println("Invalid selection!!")
  }
}