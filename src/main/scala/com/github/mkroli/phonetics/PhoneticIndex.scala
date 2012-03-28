package com.github.mkroli.phonetics
import scala.collection.mutable.HashMap

class PhoneticIndex[T <: PhoneticAlgorithm](encoder: T) {
  private var index = new HashMap[String, List[String]]

  def addWord(word: String) {
    val phonetic = encoder(word)
    index.put(phonetic, index.get(phonetic) match {
      case Some(l) => word :: l
      case None => List(word)
    })
  }

  def addWords(words: String*) =
    words foreach addWord _

  def addText(text: String) =
    addWords(text.split("\\s+"): _*)

  def apply(word: String) =
    index.getOrElse(encoder(word), Nil)

  override def toString = "PhoneticIndex(" + index.toString + ")"
}
