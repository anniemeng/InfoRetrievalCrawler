package Dictionary

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap,Set => MutSet}
import java.io.File
import scala.io.Source
import scala.math.log

object languageChecker {
  val nGramNumber = 3
  def main(args : Array[String]) = {
    checkLanguage(List())
  }

  def checkLanguage(documents: List[String]) {
    // generate dictionary n-grams
    val fileEN = "engDictionary"
    val fileDE = "deDictionary"

    val enDict = ngrams(scala.io.Source.fromFile(fileEN).mkString, nGramNumber)
    val deDict = ngrams(scala.io.Source.fromFile(fileDE).mkString, nGramNumber)

    // normalize ngram count
    val normDictEn = normalizeDict(enDict)
    val normDictDe = normalizeDict(deDict)

    if (documents.isEmpty) {
      val testStrings = List("the Bachelor’s degree programmes begin in German",
        "Das Bachelor-Studium beginnt auf Deutsch")
    } else {
      val testStrings = documents
    }

    var numEnglish = 0
    for (d <- testStrings) {
      val dGram = ngrams(d, nGramNumber)
      if (isEnglish(dGram.toMap, normDictEn, normDictDe)) {
        numEnglish += 1
        //println("is english: " + d)
      }
    }

    println("Unique English pages found: " + numEnglish)
  }

  def ngrams(doc: String, n: Int): MutMap[String,Int] = {
    if (n < 1 || doc.length < n) MutMap()
    else {
      val result = MutMap[String,Int]()
      for (ngram <- doc.sliding(n))
        result(ngram) = result.getOrElse(ngram, 0) + 1
      result
    }
  }

  def normalizeDict(dict: MutMap[String, Int]): Map[String, Double] = {
    val numGrams = dict.values.sum
    dict.mapValues(count => (count.toDouble / numGrams) + 1).toMap
  }

  def isEnglish(grams:  Map[String, Int], enDict: Map[String, Double], deDict: Map[String, Double]): Boolean = {
    var sumEN = 0.0
    var sumDE = 0.0
    for (s <- grams.keySet) {
      //println("s string:" + s)
      if (enDict.contains(s)) {
        //println("s english val: " + log(enDict(s)))
        sumEN += log(enDict(s) * grams(s))
      }
      if (deDict.contains(s)) {
        //println("s german val: " + log(deDict(s)))
        sumDE += log(deDict(s) * grams(s))
      }
    }

    //println("sum english: " + sumEN)
    //println("sum german: " + sumDE)
    sumEN > sumDE
  }

  /*def ngrams(doc: String, n: Int): Set[String] = {
  if (n < 1 || doc.length < n) Set()
  else {
    val result = mutable.Set[String]()
    for (ngram <- doc.sliding(n))
      result += ngram
    result.toSet
  }
}*/

  /*def createEstimate(doc: Set[String], dict: Set[String]) : Map[String, Double] = {
    val counts = mutable.Map[String, Int]()
    counts ++= dict.map(gram => (gram, 0))

    for(gram <- doc){
      if (dict.contains(gram)) {
        counts.put(gram, counts.getOrElse(gram, 0) += 1)
      }
    }
    val numGrams = counts.values.sum

    counts.mapValues(count => (count.toDouble + 1) / (numGrams + dict.size)).toMap
  }*/

}
