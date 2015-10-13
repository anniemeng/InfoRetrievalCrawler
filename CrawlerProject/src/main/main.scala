package main

import Crawler._
import Duplicates._
import Dictionary._

object main {
  def main(args: Array[String]) = {
    if (args.size != 1)
      throw new Exception("Provide starting URL")
    
    val docs = crawler.start(args(0))
    val uniqueDocs =  Duplicates.findDups(docs)
    languageChecker.checkLanguage(uniqueDocs)

  }  
}