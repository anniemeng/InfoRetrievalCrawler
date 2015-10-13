package Dictionary

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap}

object languageChecker {
  def main(args : Array[String]) = {
    if (args.size != 2)
      throw new Exception("Provide two files")

    // generate dictionary n-grams
    val fileEN = args(0)
    val fileDE = args(1)

    val enDict = ngrams(scala.io.Source.fromFile(fileEN).mkString, 3)
    val deDict = ngrams(scala.io.Source.fromFile(fileDE).mkString, 3)
    println(deDict.mkString("\n"))
  }

  def ngrams(doc: String, n: Int): Set[String] = {
    if (n < 1 || doc.length < n) Set()
    else {
      val result = mutable.Set[String]()
      for (ngram <- doc.sliding(n))
        result += ngram
      result.toSet
    }
  }
}
