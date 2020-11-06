/*
 * CSC 583: Asshiqgnment 2
 * Problem 7.1, 7.2
 * Author: Ashiq
 */

import scala.io.Source
import scala.collection.mutable.{HashMap, MultiMap, Set}
import scala.collection.mutable.ArrayBuffer
import scala.util.control._
import scala.math
import java.io.File
import java.util.Calendar
import collection.mutable.Map



object NaiveBayes {
  /*
   * begin: Positional Intersection Algorithm
   * finds matches of word2 to either side of word1 within k distance
   * Ashiq
   */
  implicit class EnhancedMap[A,B,C](m: Map[(A,B),C]) {
    def update(a: A, b: B, c: C) { m((a,b)) = c }
  }
  
  var spamDir = "spamDataset/spam-train"
  var nonSpamDir = "spamDataset/nonspam-train"
  var testSpamDir = "spamDataset/spam-test"
  var testNonSpamDir = "spamDataset/nonspam-test"
  
  val prior = ArrayBuffer[Double]()
  val condprob = Map[(String, Int), Double]()
  val dictionary = new HashMap[String,Int]
  
  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
  
  def ExtracVocabulary( spam: List[File], nonSpam: List[File]) : Array[String] = {
    val words = Set[String]()
    for (fileName <- spam) {
      val file = spamDir + "/" + fileName.getName
      //println(file)
      for (line <- Source.fromFile(file).getLines()) {
        // split is really slow, find faster tokenizer
        val temp = line.toString().substring(line.indexOf(" ")+1).split(' ') //foreach(dictionary.addBinding(_, i+1))
        //temp.foreach(println)
        for (wd <- temp) {
          words += wd
        }
      }
    }
    for (fileName <- nonSpam) {
      val file = nonSpamDir + "/" + fileName.getName
      //println(file)
      for (line <- Source.fromFile(file).getLines()) {
        // split is really slow, find faster tokenizer
        val temp = line.toString().substring(line.indexOf(" ")+1).split(' ') //foreach(dictionary.addBinding(_, i+1))
        for (wd <- temp) {
          words += wd
        }
      }
    }
    var i=0
    //for(dc <- dictionary) println(dictionary(dc._1))
    return words.toArray
  }
  
  def ConcatenateTextOfAllDocsInClass( spam: List[File], nonSpam: List[File], c: Int) : Array[String] = {
    val words = ArrayBuffer[String]()
    if (c == 1) {
      for (fileName <- spam) {
        val file = spamDir + "/" + fileName.getName
        //println(file)
        for (line <- Source.fromFile(file).getLines()) {
          // split is really slow, find faster tokenizer
          val temp = line.toString().substring(line.indexOf(" ")+1).split(' ')
          //temp.foreach(println)
          for (wd <- temp)
            words += wd
        }
      }
    }
    else if (c == 2) {
      for (fileName <- nonSpam) {
        val file = nonSpamDir + "/" + fileName.getName
        //println(file)
        for (line <- Source.fromFile(file).getLines()) {
          // split is really slow, find faster tokenizer
          val temp = line.toString().substring(line.indexOf(" ")+1).split(' ')
          //temp.foreach(println)
          for (wd <- temp)
            words += wd
        }
      }
    }
    return words.toArray
  }
  
  def CountTokensOfTerm(textc: Array[String], t: String) : Int = {
    var tokens = 0
    for (word <- textc) {
      if (t == word) tokens += 1
    }
    //println(tokens)
    return tokens
  }
  
  def TrainMultinomialNB ( spam: List[File], nonSpam: List[File]) : Array[String] = {
    val V = ExtracVocabulary(spam, nonSpam) // Unique Words
    //println(V.length)
    val N = spam.length + nonSpam.length
    for( c <- 1 to prior.length){
      var Nc = 0.0
      if (c==1) {Nc = spam.length} else Nc = nonSpam.length
      //println(Nc)
      prior(c-1) = (Nc/N).toDouble
      val textc = ConcatenateTextOfAllDocsInClass( spam, nonSpam ,c )
      //println(textc.length)
      val Tct = HashMap[String,Int]()
      var totCount = 0
      for (t <- V) {
        val cnt = CountTokensOfTerm(textc, t)
        Tct(t) = cnt
        //print(Tct(t) + " -> " )
        totCount += cnt
        //println(totCount)
      }
      for (t <- V) {
        condprob(t, c-1) = ((Tct(t).toDouble+1.0)/(totCount.toDouble-Tct(t).toDouble+V.length.toDouble)).toDouble
        //println( condprob(t, c-1) )
      }
    }
    return V
  }
  
  def ExtractTokensFromDoc( V: Array[String], d: String) : Array[String] = {
    //println(d)
    val tokens = ArrayBuffer[String]()
    val dict = new HashMap[String,Int]
    for (word <- V) {
      dict(word) = 1
    }
    for (line <- Source.fromFile(d).getLines()) {
      val temp = line.toString().substring(line.indexOf(" ")+1).split(' ')
      //temp.foreach(println)
      for (wd <- temp) {
        if (dict.contains(wd)) {
          tokens += wd
        }
      }
    }
    //tokens.foreach(println)
    return tokens.toArray
  }
  
  def ApplyMultinomialNB ( V: Array[String], d: String) : Int = {
    val score = ArrayBuffer[Double]()
    score.append(0.0)
    score.append(0.0)
    for( c <- 1 to prior.length) {
      val W = ExtractTokensFromDoc(V,d)
      //W.foreach(println)
      score(c-1) = scala.math.log10(prior(c-1))
      for (t <- W) {
        score(c-1) = score(c-1) + scala.math.log10( condprob(t, c-1) )
      }
    }
    //score.foreach(println)
    if (score(0) >= score (1))
      return 1
    else
      return 2
  }

  def main (args: Array[String]) {
    prior.append(1)
    prior.append(1)

    val spamFiles = getListOfFiles(spamDir)
    val nonSpamFiles = getListOfFiles(nonSpamDir)
    
    //for (file <- spamFiles) println(file.getName)
    
    val testSpamFiles = getListOfFiles(testSpamDir)
    val testNonSpamFiles = getListOfFiles(testNonSpamDir)
    
    //for (file <- testSpamFiles) println(file.getName)
    //for (file <- testNonSpamFiles) println(file.getName)
    
    val bfrTrain = Calendar.getInstance().getTime().getSeconds
    
    val V = TrainMultinomialNB ( spamFiles, nonSpamFiles)
    
    val aftTrain = Calendar.getInstance().getTime().getSeconds
    
    println ("Time required to train: " + (scala.math.abs(aftTrain-bfrTrain)))
    //V.foreach(println)
    
    val score = ArrayBuffer[Int]()
    
    var TP = 0.0
    var FP = 0.0
    var FN = 0.0
    var TN = 0.0
    
    //val sc = ApplyMultinomialNB (V, testSpamDir + "/" + testSpamFiles(0).getName)
    
    for (file <- testSpamFiles) {
      val sc = ApplyMultinomialNB (V, testSpamDir + "/" + file.getName)
      score += sc
      if (sc == 1) TP += 1
      else FN += 1
    }
    
    for (file <- testNonSpamFiles) {
      val sc = ApplyMultinomialNB (V, testNonSpamDir + "/" + file.getName)
      score += sc
      if (sc == 2) TN += 1
      else FP += 1
    }
    
    val accuracy = ((TP+TN)/(TP+TN+FP+FN)).toDouble
    println("Accuracy: " + accuracy)
    
    println("Results with respect to Spam lebel:")
    val P = (TP/(TP+FP)).toDouble
    println("Precision: " + P)
    val R = (TP/(TP+FN)).toDouble
    println("Recall: " + R)
    
    val F1 = ((2.0*P*R)/(P+R)).toDouble
    println("F1 Score: " + F1)
  }
  /*
   * end: Main method
   */
}