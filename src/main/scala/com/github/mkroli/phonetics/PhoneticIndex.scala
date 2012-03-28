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
