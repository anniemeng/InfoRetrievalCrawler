package Dictionary
import java.io.{PrintWriter}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import scala.collection.JavaConversions._

object generateDictionary {
  def main(args : Array[String]) = {
    val fileEN = Jsoup.connect("http://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:52009DC0487&from=EN").get()
    val engDoc = obtainString(fileEN)
    new PrintWriter("engDictionary") { write(engDoc); close }

    val fileDE = Jsoup.connect("http://eur-lex.europa.eu/legal-content/DE/TXT/HTML/?uri=CELEX:52009DC0487&from=EN").get()
    val deDoc = obtainString(fileDE)
    new PrintWriter("deDictionary") { write(deDoc); close }
    println("finished")
  }

  def obtainString(doc: Document) : String = {
    val text = doc.getElementsByTag("p")
    val s = new StringBuilder
    val appendix = "(\\[\\d{1,}\\])(.*)+"
    for (x <- text) {
      // remove appendix
      if (x.text().matches(appendix)) {
        s ++= ""
      }
      else {
        s ++= x.text().replaceAll("([^a-zA-Z\\s])", "")
        s ++= "\n"
      }
    }
    s.toString()
  }
}
