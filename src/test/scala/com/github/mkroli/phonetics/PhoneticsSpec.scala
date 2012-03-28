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
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec

@RunWith(classOf[JUnitRunner])
class PhoneticsSpec extends Spec {
  describe("KoelnerPhonetics") {
    it("match given combinations") {
      assert("Müller-Lüdenscheidt".koelnerPhonetics === "65752682")
      assert("Wikipedia".koelnerPhonetics === "3412")
      assert("Bademeister".koelnerPhonetics === "126827")
      assert("Lump".koelnerPhonetics === "561")
      assert("Morph".koelnerPhonetics === "673")
      assert("Pumpe".koelnerPhonetics === "161")
      assert("Satz".koelnerPhonetics === "88")
      assert("Cha-Cha-Cha".koelnerPhonetics === "444")
      assert("Xylophon".koelnerPhonetics === "48536")
      assert("Haxe".koelnerPhonetics === "048")
      assert("C".koelnerPhonetics === "8")
      assert("nc".koelnerPhonetics === "68")
      assert("HackXaver".koelnerPhonetics === "04837")
    }
  }

  describe("Soundex") {
    it("match given combinations") {
      assert("Wikipedia".soundex === "W213")
      assert("Robert".soundex === "Rupert".soundex)
      assert("Robert".soundex === "R163")
      assert("alone...".soundex === "A450")
    }
  }
}
