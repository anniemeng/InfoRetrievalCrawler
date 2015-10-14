package Crawler
import org.jsoup.Jsoup
import org.jsoup.Connection
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import scala.collection.mutable.Queue
import java.io._

object Crawler {

  def getLinks(s: String) : List[String] = {
    
    var linksSet = scala.collection.mutable.Set[String]()
    var textList = scala.collection.mutable.ListBuffer[String]()
   
    var doc : Document = Jsoup.connect(s).get()
    var links : Elements = doc.select("a[href]")
    val iter = links.iterator()
    
    while(iter.hasNext) {
      var temp : String = iter.next().attr("abs:href").replaceAll("\\#.*$", "")
      temp = temp.replaceAll("\\?.*$", "")
      if (temp.matches("""^(http|https)://idvm-infk-hofmann03\.inf\.ethz\.ch/.*\.html$"""))
        linksSet += temp
    }

    val linksQueue = scala.collection.mutable.Queue(linksSet.toList: _*)
    var len = linksSet.size //Initial Number of Unique URL's
    var ErrorFlag = 0
    while(linksQueue.nonEmpty) {
      val url = linksQueue.dequeue()
      try {
        val response : Connection.Response = Jsoup.connect(url).execute()
        doc = response.parse()
        var text : String = doc.body().text()
        textList += text
        links = doc.select("a[href]")
        val iter = links.iterator()
        while(iter.hasNext) {//Iterate over all the URLs in the webpage
          var str : String =  iter.next().attr("abs:href").replaceAll("\\#.*$", "")
          str = str.replaceAll("\\?.*$","")
          if (str.matches("""^(http|https)://idvm-infk-hofmann03\.inf\.ethz\.ch/.*\.html$"""))
            linksSet += str
          if (linksSet.size > len) {// If the URL is new, enqueue it.
            linksQueue.enqueue(str)
            len = len + 1
          }
        }      
       }
       catch {
         case ex: IOException => {
           ErrorFlag = 1
         }
       }
    }
    var uniqueURL = textList.size
    if (ErrorFlag == 1)//404 page accounts as one URL
      uniqueURL = uniqueURL + 1
    println("Number of Unique URLs found: " + uniqueURL)
    textList.toList
  }
}