package Dictionary

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap}
import java.io.File
import scala.io.Source
import scala.math.log

object languageChecker {
  val nGramNumber = 3
  def main(args : Array[String]) = {
    if (args.size != 2)
      throw new Exception("Provide two files")

    // generate dictionary n-grams
    val fileEN = args(0)
    val fileDE = args(1)

    val enDict = ngrams(scala.io.Source.fromFile(fileEN).mkString, nGramNumber)
    val deDict = ngrams(scala.io.Source.fromFile(fileDE).mkString, nGramNumber)
    //println(deDict.mkString("\n"))

    //val normDict =

    val testStrings = List("a rose is a rose is a rose",
      "a rose is a rose is a rose a","the documents","the documents","a rose")

    var numEnglish = 0
    // split by new line?
    for (d <- testStrings) {
      val dGram = ngrams(d, nGramNumber)
      val estimateEN = createEstimate(dGram, enDict)
      val estimateDE = createEstimate(dGram, deDict)
      if (isEnglish(dGram, estimateEN, estimateDE)) {
        numEnglish += 1
      }
    }
  }

  def ngrams(doc: String, n: Int): Map[String,Int] = {
    if (n<1 || doc.length<n) Map()
    else {
      val result = MutMap[String,Int]()
      for (ngram <- doc.sliding(n))
        result(ngram) = result.getOrElse(ngram,0)+1
      result.toMap
    }
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

  def normalizeDict()

  def createEstimate(doc: Set[String], dict: Set[String]) : Map[String, Double] = {
    val counts = mutable.Map[String, Int]()
    counts ++= dict.map(gram => (gram, 0))

    for(gram <- doc){
      if (dict.contains(gram)) {
        counts.put(gram, counts.getOrElse(gram, 0) += 1)
      }
    }
    val numGrams = counts.values.sum

    counts.mapValues(count => (count.toDouble + 1) / (numGrams + dict.size)).toMap
  }

  def isEnglish(strings: Set[String], enDict: Map[String, Double], deDict: Map[String, Double]): Boolean = {
    for (s <- strings) {
      if (enDict.contains(s)) {

      }
    }
    false
  }
}
