import scala.collection.mutable.{Map => MutMap}

object Dublicates {
  type Shingle = List[String]
  
  def main(args: Array[String]) {
    val duplicates = findDups(List("a rose is a rose is a rose","of all","the documents"))
  }
  
  def findDups(docs: List[String]): Int = {
    val shingleDocs = docs.map(d => shingle(d,3))
    val simHashes = shingleDocs.map { x => simhash(x) }
    println(simHashes)
    return 0
  }
  
  def simhash(shingleDoc: Set[Shingle]): Int = {
    val simpleHashes = shingleDoc.map { x => x.hashCode() }
    var G = new Array[Int](32)    // Int size enough?
    simpleHashes.foreach { x => 
      (
          for(j <- 0 to 31) {
            G(j) += 2*(x>>j & 1) - 1
          }
      )}

    var simHash : Int = 0
    for (j <- 0 to 31) {
      simHash = simHash | (((G(j).signum + 1) / 2) << j)
    }
    simHash
  }
  
  def shingle(tks: List[String], q: Int): Set[Shingle] = {
    require(q>=1)
    tks.sliding(q).toSet
  }
  
  def shingle(doc: String, q: Int): Set[Shingle] = {
    shingle(doc.split("[ .,;:?!\t\n\r\f]+").toList,q)
  }
}