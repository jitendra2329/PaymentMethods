package com.knoldus

import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import scala.util.{Try, Success, Failure}

trait PaymentProcess {
  def makePayment(method: String): Unit
}

case class PayPal(email: String)

case class CreditCard(cardNumber: Long, expiry: String)

case class BankTransfer(accountNumber: Long, accountHolderName: String, ifsc: String)

class PaymentServices extends PaymentProcess {
  private var accountBalance = 500000.0
  val paymentMethods: ListBuffer[Object] = ListBuffer().empty

  def seeBalance(): Double = {
    accountBalance
  }

  // for adding PayPal as one of your payment method
  def addPayPal(): Unit = {
    println("Enter Details:")
    print("Email: ")
    val email = StdIn.readLine()

    paymentMethods += PayPal(email)
    println("Method added!")
    println("1. List all methods\n2. Make payment")
    val choice = Try(StdIn.readInt())
    choice match {
      case Success(value) => value match {
        case 1 =>
          println(paymentMethods.toList)
          println("1. See Account balance\n2. Make payment")
          val selection = StdIn.readInt()
          if (selection == 1) println(seeBalance())
          else makePayment("payPal")

        case 2 => makePayment("payPal")
        case _ => println("Invalid Selection!!")
      }
      case Failure(exception) => println("Invalid Selection!!")
    }
  }

  // for adding Credit Card as one of your payment method
  def creditCard(): Unit = {
    println("Enter Details:")
    print("Card number: ")
    val cardNumber = StdIn.readLong()
    print("\nExpiry date: ")
    val expiryDate = StdIn.readLine()

    paymentMethods += CreditCard(cardNumber, expiryDate)
    println("Method added!")
    println("1. List all methods\n2. Make payment")
    val choice = Try(StdIn.readInt())
    choice match {
      case Success(value) => value match {
        case 1 =>
          println(paymentMethods.toList)
          println("1. See Account balance\n2. Make payment")
          val selection = StdIn.readInt()
          if (selection == 1) println(seeBalance())
          else makePayment("creditCard")

        case 2 => makePayment("creditCard")
        case _ => println("Invalid Selection!!")
      }
      case Failure(exception) => println("Invalid Selection!!")
    }
  }

  // for adding through Bank as one of your payment method
  def addBankDetail(): Unit = {
    println("Enter Details:")

    print("Account number: ")
    val accountNumber = StdIn.readLong()
    print("\nIFSC code: ")
    val ifsc = StdIn.readLine()
    print("\nAccount holder name: ")
    val name = StdIn.readLine()

    paymentMethods += BankTransfer(accountNumber, name, ifsc)

    println("Method added!")
    println("1. List all methods\n2. Make payment")

    val choice = Try(StdIn.readInt())

    choice match {
      case Success(value) => value match {
        case 1 =>
          println(paymentMethods.toList)
          println("1. See Account balance\n2. Make payment")
          val selection = StdIn.readInt()
          if (selection == 1) println(seeBalance())
          else makePayment("bankTransfer")

        case 2 => makePayment("bankTransfer")
        case _ => println("Invalid Selection!!")
      }
      case Failure(exception) => println("Invalid Selection!!")
    }
  }

  // for making payments after adding payment methods
  override def makePayment(method: String): Unit = {
    print("Enter amount: ")

    val amount = StdIn.readDouble()

    if (amount <= accountBalance) {
      method match {
        case "creditCard" => paymentThroughPaypal(amount)
        case "bankTransfer" => paymentThroughBank(amount)
        case "payPal" => paymentThoughCreditCard(amount)
        case _ => println("Method not specified")
      }
    } else println("Insufficient balance!!")
  }

  // Making payment via PayPal
  private def paymentThroughPaypal(amount: Double): Unit = {
    val chargeForCreditCard = 2 //percent
    val paymentCharge = (amount * chargeForCreditCard) / 100
    val amountToDeduction = amount + paymentCharge
    val availableBalance = accountBalance - amountToDeduction
    accountBalance = availableBalance
    println(s"Payment successful through Credit Card\nAvailable balance is ${accountBalance}")
    println(s"Payment charge: $paymentCharge")
  }

  // Making payment via Bank
  private def paymentThroughBank(amount: Double): Unit = {
    val chargeForBankTransfer = 1.5 //percent
    val paymentCharge = (amount * chargeForBankTransfer) / 100
    val amountToDeduction = amount + paymentCharge
    val availableBalance = accountBalance - amountToDeduction
    accountBalance = availableBalance
    println(s"Payment successful through Credit Card\nAvailable balance is ${accountBalance}")
    println(s"Payment charge: $paymentCharge")
  }

  // Making payment via Credit Card
  private def paymentThoughCreditCard(amount: Double): Unit = {
    val chargeForPayPal = 2.5 //percent
    val paymentCharge = (amount * chargeForPayPal) / 100
    val amountToDeduction = amount + paymentCharge
    val availableBalance = accountBalance - amountToDeduction
    accountBalance = availableBalance
    println(s"Payment successful through PayPal\nAvailable balance is ${accountBalance}")
    println(s"Payment charge: $paymentCharge")
  }

  // for selecting payment method for the transaction
  def selectPaymentMethod(): Unit = {
    println("Select payment method:\n1. PayPal\n2. Credit Card\n3. Bank Transfer")
    val method = Try(StdIn.readInt())
    method match {
      case Success(value) => value match {
        case 1 => makePayment("creditCard")
        case 2 => makePayment("bankTransfer")
        case 3 => makePayment("payPal")
        case _ => println("Invalid Selection!!")
      }
      case Failure(e) => println("Invalid Selection!!")
    }
  }

  // For adding a new Payment method
  def addPaymentMethod(): Unit = {
    println("Select method to add:\n1. PayPal\n2. Credit Card\n3. Bank Transfer")
    val method = Try(StdIn.readInt())
    method match {
      case Success(value) => value match {
        case 1 => addPayPal()
        case 2 => creditCard()
        case 3 => addBankDetail()
        case _ => println("Invalid Selection!!")
      }
      case Failure(e) => println("Invalid Selection!!")
    }
  }
}

