package Duplicates

import java.io.FileWriter

import scala.collection.mutable.{Map => MutMap, Set => MutSet}
import org.jsoup.nodes.Document
import scala.collection.JavaConversions._

object Duplicates {
  type Shingle = List[String]
  type pBits = Array[Int]  //random permuted hash bits
  def HammingDistance = 2
  def bucketBits = 4
  def p = 100 // # permutations

  def findDups(docs: List[String]): MutSet[String] = {
    val cleanDocs = docs.map(doc => doc.split("[ .,;:?!\t\n\r\f]+").toList)
    val shingleDocs = cleanDocs.map(d => shingle(d,3))
    val simHashes = shingleDocs.map { x => simhash(x) }

    val permutations = createPermutations(p,bucketBits)
    val bucketSize = 32*bucketBits   // bucket could exist of [bucketBits] times the same bit

    //initialize p empty buckets of size bucketSize
    var buckets = Array.ofDim[MutSet[Int]](p,bucketSize)
    for (i <- 0 to (p-1)) {
      for (j <- 0 to (bucketSize-1)) {
        buckets(i)(j) = MutSet[Int]()
      }
    }
    val fw = new FileWriter("nearUrls.txt",true)
    //hash into buckets and count duplicates
    var duplicates = 0
    var near = 0
    var uniqueDocSet = MutSet[List[String]]()
    var output = MutSet[String]()
    for ( i <- 0 to (simHashes.length - 1)) {
      val dupOrNear = hashDoc(simHashes(i), permutations, buckets)
      duplicates += dupOrNear._1
      near += dupOrNear._2
      if (dupOrNear._1 == 0 && dupOrNear._2 == 0) {
        uniqueDocSet.add(cleanDocs(i))
        output.add(docs(i))
      } else {
        fw.write(docs(i))
        fw.write("\n \n \n \n \n \n")
      }
    }
    fw.close()
    println("Exact duplicates found: " + duplicates)
    println("Near duplicates found: " + near)

    findStudent(uniqueDocSet)

    return output
  }

  def findStudent(doc: MutSet[List[String]]): Unit = {
    val student = "student$"
    var count = 0
    for (d <- doc) {
      d.filter(s => s.matches(student))
      count += d.size
    }
    println("Term frequency of \"student\": " + count)
  }

  def hashDoc(docHash: Int, permutations: Array[pBits], buckets: Array[Array[MutSet[Int]]]): (Int,Int) = {
    // return (duplicates, near)
    for (b <- 0 to (p-1)) {
      val hashBits = permutations(b) // hash bits for bucket b

      var key: Int = 0 //select hashBits from docHash and create bucket index integer
      for (i <- 0 to (bucketBits-1)) {
        key = key | (((docHash>>hashBits(i)) & 1) << i)
      }

      // hash into buckets(b)(key)
      if (!buckets(b)(key).isEmpty){
        // duplicate candidate(s) found -> check them completely
        // exact and near compare
        if (buckets(b)(key).contains(docHash)) {
          return (1,0)
        } else if (nearCheck(docHash,buckets(b)(key),HammingDistance)){
          return (0,1)
        } else {
          buckets(b)(key).add(docHash)
        }
      } else {
        buckets(b)(key).add(docHash)
      }
    }
    return (0,0)
  }

  def nearCheck(a: Int, s: MutSet[Int], hammingDistance: Int): Boolean = {
    for (b <- s) {
      var h = 0
      for(i <- 0 to 31) {
        if ((((a>>i) ^ (b>>i)) & 1) == 1) {   // ^ = bitwiseXOR
          h += 1
          if (h > hammingDistance) {
            return false
          }
        }
      }
    }
    return true
  }

  def createPermutations(k: Int, p: Int): Array[pBits] = {
    val r = scala.util.Random
    var perms = new Array[pBits](k)
    for(i <- 0 to k-1) {
      var ps = new pBits(p)
      for(j <- 0 to p-1) {
        ps(j) = r.nextInt(32) // could yield non distinct bit-array
      }
      perms(i) = ps
    }
    perms
  }

  def simhash(shingleDoc: Set[Shingle]): Int = {
    //see slide 40 lecture 2
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
}