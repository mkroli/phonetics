/*
 * Copyright 2012 Michael Krolikowski
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.github.mkroli.phonetics

object KoelnerPhonetics extends PhoneticAlgorithm {
  override def phonetic(s: String) = {
    s.toUpperCase.toSeq.contextFlatMap {
      case (_, 'A' | 'E' | 'I' | 'J' | 'O' | 'U' | 'Y', _) => Seq(0)
      case (_, 'H', _) => Nil
      case (_, 'B', _) => Seq(1)
      case (_, 'P', None) => Seq(1)
      case (_, 'P', Some(f)) if !("H" contains f) => Seq(1)
      case (_, 'D' | 'T', None) => Seq(2)
      case (_, 'D' | 'T', Some(f)) if !("CSZ" contains f) => Seq(2)
      case (_, 'F' | 'V' | 'W', _) => Seq(3)
      case (_, 'P', Some('H')) => Seq(3)
      case (_, 'G' | 'K' | 'Q', _) => Seq(4)
      case (None, 'C', Some('A' | 'H' | 'K' | 'L' | 'O' | 'Q' | 'R' | 'U' | 'X')) => Seq(4)
      case (Some(p), 'C', Some('A' | 'H' | 'K' | 'O' | 'Q' | 'U' | 'X')) if !("SZ" contains p) => Seq(4)
      case (None, 'X', _) => Seq(4, 8)
      case (Some(p), 'X', _) if !("CKQ" contains p) => Seq(4, 8)
      case (_, 'L', _) => Seq(5)
      case (_, 'M' | 'N', _) => Seq(6)
      case (_, 'R', _) => Seq(7)
      case (_, 'S' | 'Z', _) => Seq(8)
      case (Some('S' | 'Z'), 'C', _) => Seq(8)
      case (None, 'C', None) => Seq(8)
      case (None, 'C', Some(f)) if !("AHKLOQRUX" contains f) => Seq(8)
      case (_, 'C', None) => Seq(8)
      case (_, 'C', Some(f)) if !("AHKOQUX" contains f) => Seq(8)
      case (_, 'D' | 'T', Some('C' | 'S' | 'Z')) => Seq(8)
      case (Some('C' | 'K' | 'Q'), 'X', _) => Seq(8)
      case _ => Nil
    }.contextFlatMap {
      case (Some(p), c, _) if (p == c) => Nil
      case (_, c, _) => Seq(c)
    }.contextFlatMap {
      case (Some(_), c, _) if (c == 0) => Nil
      case (_, c, _) => Seq(c)
    }.mkString
  }
}
