/*
 * UCLID5 Verification and Synthesis Engine
 *
 * Copyright (c) 2017.
 * Sanjit A. Seshia, Rohit Sinha and Pramod Subramanyan.
 *
 * All Rights Reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 1. Redistributions of source code must retain the above copyright notice,
 *
 * this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 *
 * documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its
 * contributors may be used to endorse or promote products derived from this
 * software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Author: Alejandro Sanchez Ocegueda
 *
 * UCLID oracle tests
 *
 */

package uclid
package test

import org.scalatest.flatspec.AnyFlatSpec
import java.io.File
import uclid.{lang => l}

object SmokeSpec {
  def expectedWarns(filename: String, nWarn : Int) : String = {
    UclidMain.enableStringOutput()
    UclidMain.clearStringOutput()
    val config = UclidMain.Config().copy(smoke=true)
    val modules = UclidMain.compile(ConfigCons.createConfig(filename), lang.Identifier("main"), true)
    val mainModule = UclidMain.instantiate(config, modules, l.Identifier("main"))
    assert (mainModule.isDefined)
    val results = UclidMain.execute(mainModule.get, config)
    val outputString = UclidMain.stringOutput.toString()

    var reachableLines : List[String] = Nil
    var unreachableLines : List[String] = Nil
    var undeterminedLines : List[String] = Nil

    results.foreach { (p) =>
      if (p.result.isTrue) {
        unreachableLines = p.assert.name +: unreachableLines
      } else if (p.result.isFalse) {
        reachableLines = p.assert.name +: reachableLines
      } else {
        undeterminedLines = p.assert.name +: undeterminedLines
      }
    }

    var reachableSet : Set[String] = reachableLines.toSet
    var unreachableSet : Set[String] = unreachableLines.toSet
    var undeterminedSet : Set[String] = undeterminedLines.toSet

    unreachableSet = unreachableSet.diff(reachableSet)
    undeterminedSet = undeterminedSet.diff(reachableSet)
    undeterminedSet = undeterminedSet.diff(unreachableSet)
    reachableSet = reachableSet.diff(unreachableSet)
    reachableSet = reachableSet.diff(undeterminedSet)

    // UclidMain.printResult("%d smoke tests run.".format(assertionResults.size))
    // printf("%d code blocks tested.".format(reachableSet.size + unreachableSet.size + undeterminedSet.size))
    // printf("%d warnings.".format(unreachableSet.size))
    // printf("%d tests inconclusive.".format(undeterminedSet.size))

    var warnCount : Int = unreachableSet.size
    var undetCount : Int = undeterminedSet.size

    assert (warnCount == nWarn)

    assert (undetCount == 0)

    outputString
  }
}
class SmokeSpec extends AnyFlatSpec {
  
  "test-smoke-bmc.ucl" should "warn about two blocks." in {
    SmokeSpec.expectedWarns("./test/test-smoke-bmc.ucl", 2)
  }
  "test-smoke-bmc-fixed.ucl" should "output no warnings." in {
    SmokeSpec.expectedWarns("./test/test-smoke-bmc-fixed.ucl", 0)
  }
  "test-smoke-induction.ucl" should "output no warnings." in {
    SmokeSpec.expectedWarns("./test/test-smoke-induction.ucl", 0)
  }
  "test-smoke-ltl.ucl" should "warn about one block." in {
    SmokeSpec.expectedWarns("./test/test-smoke-ltl.ucl", 1)
  }
  "test-smoke-multiple-modules.ucl" should "warn about one block." in {
    SmokeSpec.expectedWarns("./test/test-smoke-multiple-modules.ucl", 1)
  }
}
