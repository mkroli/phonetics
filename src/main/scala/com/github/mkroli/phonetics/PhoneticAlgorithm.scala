package com.github.mkroli.phonetics

trait PhoneticAlgorithm {
  def phonetic(word: String): String

  def apply(s: String) = phonetic(s)
}
