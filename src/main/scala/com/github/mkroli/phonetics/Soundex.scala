package com.github.mkroli.phonetics

object Soundex extends PhoneticAlgorithm {
  override def phonetic(s: String) = {
    s.toUpperCase.toSeq.contextFlatMap {
      case (None, c, _) => Seq(c)
      case (_, 'A' | 'E' | 'I' | 'O' | 'U' | 'H' | 'W' | 'Y', _) => Seq(0)
      case (_, 'B' | 'F' | 'P' | 'V', _) => Seq(1)
      case (_, 'C' | 'G' | 'J' | 'K' | 'Q' | 'S' | 'X' | 'Z', _) => Seq(2)
      case (_, 'D' | 'T', _) => Seq(3)
      case (_, 'L', _) => Seq(4)
      case (_, 'M' | 'N', _) => Seq(5)
      case (_, 'R', _) => Seq(6)
      case _ => Nil
    }.contextFlatMap {
      case (Some(p), c, _) if (p == c) => Nil
      case (_, c, _) => Seq(c)
    }.contextFlatMap {
      case (_, c, _) if (c == 0) => Nil
      case (_, c, _) => Seq(c)
    }.take(4).padTo(4, 0).mkString
  }
}
