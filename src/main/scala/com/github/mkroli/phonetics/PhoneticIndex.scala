package com.github.mkroli.phonetics
import scala.collection.mutable.HashMap

class PhoneticIndex(encoder: PhoneticAlgorithm) {
  private var index = new HashMap[String, List[String]]

  def addWord(word: String) {
    val phonetic = encoder(word)
    index.put(phonetic, index.get(phonetic) match {
      case Some(l) => word :: l
      case None => List(word)
    })
  }

  override def toString = "PhoneticIndex(" + index.toString + ")"
}
