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
package com.github.mkroli

package object phonetics {
  implicit def stringToPhoneticsString(s: String) = new PhoneticsString(s)

  class PhoneticsString(s: String) {
    def koelnerPhonetics = KoelnerPhonetics(s)

    def soundex = Soundex(s)
  }

  implicit private[phonetics] def seqToRichSeq[T](s: Seq[T]) = new RichSeq(s)

  private[phonetics] class RichSeq[T](s: Seq[T]) {
    def contextFlatMap[O](fun: (Option[T], T, Option[T]) => Seq[O]): Seq[O] = {
      for {
        i <- 0 until s.size
        m <- fun(if (i > 0) Some(s(i - 1)) else None,
          s(i),
          if (i + 1 < s.length) Some(s(i + 1)) else None)
      } yield m
    }
  }
}
