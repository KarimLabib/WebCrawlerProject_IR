

import scala.collection.mutable.{Map => MutMap}
import scala.collection.mutable.{Map => MutMap}

object LanguageDetector {
  
  def main(args: Array[String]) {
    
    def ngrams(doc : String, n : Int): Map[String, Double] = {
      if (n < 1 || doc.length < n) Map()
      else {
        var result = MutMap[String, Double]()
        for (ngram <- doc.sliding(n))
          result(ngram) = result.getOrElse(ngram, 0.0) + 1
        
        var sum : Double = 0
        result.foreach(sum += _._2)
        result = result.map{case(x, y) => (x, Math.log(y.toDouble/sum))}
        result.toMap
      }
    }
    
    def langSearch(doc : String, dataEn : Map[String, Double], dataDe: Map[String, Double], n : Int): (Double, Double) = {
      var resEn : Double = 0
      var resDe : Double = 0

      for (ngram <- doc.sliding(n)) {
        resEn += dataEn.getOrElse(ngram, 0.0)
        resDe += dataDe.getOrElse(ngram, 0.0)
      }

      var result = (-resEn, -resDe)
      result
    }
    
    val germanCollection : String = io.Source.fromFile("src/german_train.txt").mkString
    val englishCollection : String = io.Source.fromFile("src/english_train.txt").mkString
    val germanTest : String = io.Source.fromFile("src/german_test.txt").mkString
    val englishTest : String = io.Source.fromFile("src/english_test.txt").mkString
    
    val deGrams = ngrams(germanCollection, 4)
    val enGrams = ngrams(englishCollection, 4)

    println(langSearch(germanTest, enGrams, deGrams, 4))
    println(langSearch(englishTest, enGrams, deGrams, 4))

  }

}