package main.scala

object Poker extends App {

  /*
   * Given a set of 5 playing card identifiers such as 2H, 7C, QS, 10D, 2D;
   * determine if this hand is better than some other hand, according to the rules of poker.
   *
   * Hands will be a string with 5 cards comma separated,
   * each card will have 1-2 digits or JQKA and a suit indicator C,D,S,H (i.e. 10C, KH)
   *
   * Possible Hand Types Below:
   *   Straight flush
   *   Four of a kind
   *   Full house
   *   Flush
   *   Straight
   *   Three of a kind
   *   Two pair
   *   One pair
   *
   * The goal of this is to compare between the hand types.
   * Comparing 2 of the same type (i.e. 2 straights) to determine a winner is outside the scope
   * and will not be tested.
   *
   * Implement hand1WinsOverHand2 method and return whether or not the first hand wins over the second hand.
   */
  def makeCard(card: Array[String]): Array[String] = {
    var singleCardArray = new Array[String](0)
    if (card.length == 3) {
      var number = card(0) + card(1);
      singleCardArray :+= number
      singleCardArray :+= card(2)
    }else if(card(0) == "A" || card(0) == "K" || card(0) == "Q" || card(0) == "J" ) {
      if(card(0) == "A"){
        singleCardArray :+= "14"
      }
      else if (card(0) == "K"){
        singleCardArray :+= "13"
      }
      else if(card(0) == "Q"){
        singleCardArray :+= "12"
      }
      else if (card(0) == "J"){
        singleCardArray :+= "11"
      }
      singleCardArray :+= card(1)
    }
    else{
      singleCardArray :+= card(0)
      singleCardArray :+= card(1)
    }
    return singleCardArray
  }

  def bubbleSort(a:Array[Int]):Array[Int]= {
    for (i <- 1 to a.length - 1) {
      for (j <- (i - 1) to 0 by -1) {
        if (a(j) > a(j + 1)) {
          val temp = a(j + 1)
          a(j + 1) = a(j)
          a(j) = temp
        }
      }
    }
    return a
  }

    def checkForFlush(hand: Array[Array[String]]): Int = {
      var numbersForSort = new Array[Int](0)
      var sameSuite = false
      var suite = hand(0)(1)
      for(i <- 0 to 4 ) {
        if (hand(i)(1) != suite) {
          sameSuite = false
          numbersForSort :+= hand(i)(0).toInt
        } else if(hand(i)(1) == suite) {
          sameSuite = true
          numbersForSort :+= hand(i)(0).toInt
        }
      }
      var sortedNumbers = bubbleSort(numbersForSort)
      var currentNumber = sortedNumbers(0)
      for(i <- 1 to 4) {
        if (currentNumber + 1 == sortedNumbers(i) && sameSuite == true) {
          currentNumber += 1
          return 8
        }
        else if(currentNumber + 1 == sortedNumbers(i) && sameSuite == false){
          return 4
        }
        else if(currentNumber +1 != sortedNumbers(i) && sameSuite == true){
          return 5
        }
      }
      return 0
    }

    def checkForHowManyOfAKind(hands: Array[Array[String]]): Int = {
      var howManyOfAKind = new Array[Int](0)
      var onlyNumbers = new Array[Int](0)
      for (i <- 0 to hands.length -1) {
        onlyNumbers :+= hands(i)(0).toInt
        }
      bubbleSort(onlyNumbers)
      for(i <- 1 to onlyNumbers.length -1){
        var matches =  onlyNumbers.count(_ == onlyNumbers(i))
        if(matches > 1){
          if (i > 1 && (onlyNumbers(i) != onlyNumbers(i -1))){
            howManyOfAKind :+= matches
          }else if(i == 1){
            howManyOfAKind :+= matches
          }
        }
    }
      if(howManyOfAKind.length == 1 && howManyOfAKind(0) == 4){
        return 7
      }else if(howManyOfAKind.length == 1 && howManyOfAKind(0) == 3){
        return 3
      }else if(howManyOfAKind.length ==1 && howManyOfAKind(0) == 2){
        return 1
      }else if(howManyOfAKind.length == 2 && howManyOfAKind(0) == 2 && howManyOfAKind(1) == 2){
        return 2
      }else if (howManyOfAKind.length > 0) {
        return 6
      }
      return 0
    }

  def splitCards(allCards: String): Array[Array[String]] = {
    var allHandCardsSplit = new Array[Array[String]](0)
    var splitHand =  allCards.split(",")
    splitHand.foreach { f =>
    val singleCard = f.split("")
    allHandCardsSplit :+= makeCard(singleCard)
  }
  return allHandCardsSplit
  }

  def hand1WinsOverHand2(hand1Str: String, hand2Str: String): Boolean = {
    var hand1PointValue = 0
    var hand2PointValue = 0
    var allHand1CardsSplit = splitCards(hand1Str)
    var allHand2CardsSplit = splitCards(hand2Str)
    var isHand1Flush = checkForFlush(allHand1CardsSplit)
    var isHand2Flush = checkForFlush(allHand2CardsSplit)
    if(isHand1Flush > 0){
      hand1PointValue = isHand1Flush
    }else{
      hand1PointValue = checkForHowManyOfAKind(allHand1CardsSplit)
    }
    if(isHand2Flush > 0){
      hand2PointValue = isHand2Flush
    }else{
      hand2PointValue = checkForHowManyOfAKind(allHand2CardsSplit)
    }
    if(hand1PointValue > hand2PointValue){
      return true
    }
    return false
  }

  implicit class CompareTwoPokerHands(hand1: String) {
    def winsOver(hand2: String): Unit = {
      val result = if (hand1WinsOverHand2(hand1, hand2)) "Correct" else "Incorrect"
      println(s"$result, hand [$hand1] wins over [$hand2]")
    }
  }

  println("Poker Hand comparison")
  "8C,9C,10C,JC,QC" winsOver "6S,7H,8D,9H,10D" // straight flush
  "4H,4D,4C,4S,JS" winsOver "6C,6S,KH,AS,AD" // four of a kind
  "5C,3C,10C,KC,7C" winsOver "6C,6D,6H,9C,KD" // flush
  "4H,4D,4C,KC,KD" winsOver "9D,6S,KH,AS,AD" // full house
  "2C,3C,4S,5S,6S" winsOver "6C,6D,6H,9C,KD" // straight
  "7C,7D,7S,3H,4D" winsOver "9S,6S,10D,AS,AD" // three of a kind
  "8C,8H,10S,KH,KS" winsOver "2S,2D,JH,7S,AC" // two pair
  "AC,AH,3C,QH,10C" winsOver "3S,2D,KH,JS,AD" // one pair
  System.exit(0)
}