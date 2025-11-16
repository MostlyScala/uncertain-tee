/*
 * Copyright 2025 Mostly Codes
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

package mostly.uncertaintee.scalacheck.ops

import mostly.uncertaintee.*
import org.scalacheck.Prop

trait PropUncertainOps {

  extension (prop: Prop.type) {

    /** Creates a property that checks an Uncertain-based predicate.
      *
      * @param uncertain
      *   The uncertain distribution to sample from
      * @param predicate
      *   The property to check on each sample
      * @param samples
      *   Number of samples to check
      */
    def forAllUncertain[T](
      uncertain: Uncertain[T],
      samples: Int = 1000
    )(predicate: T => Boolean): Prop = Prop { _ =>
      val results: List[T]                        = List.fill(samples)(uncertain.sample())
      val (successes: List[T], failures: List[T]) = results.partition(predicate(_))

      if (failures.isEmpty) {
        Prop.Result(
          status = Prop.True
        )
      } else {
        Prop.Result(
          status = Prop.False,
          labels = Set(
            s"Failed $failures out of $samples samples;\n\t" +
              s"Failed values were: ${failures.mkString("[", ",", "]")}.\n\t" +
              s"Succeeded values were: ${successes.mkString("[", ",", "]")}"
          )
        )
      }
    }
  }

}
