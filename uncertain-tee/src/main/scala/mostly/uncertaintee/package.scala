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

package mostly

import mostly.uncertaintee.ops.distribution.AllDistributionOps

package object uncertaintee {

  /** Allows access to syntax for operating on [[Uncertain]]
    *
    * {{{
    *   // recommended, let the compiler sort out your imports instead
    *   // of picking-and-mixing
    *   import mostly.uncertaintee.syntax.*
    *   // ... equivalent to importing .all (which is familiar to many libraries), which is also supported
    *   import mostly.uncertaintee.syntax.all.*
    *
    *   // If you want to pick-and-mix:
    *   import mostly.uncertaintee.syntax.all.*
    *   import mostly.uncertaintee.syntax.arithmetic.*
    *   import mostly.uncertaintee.syntax.boolean.*
    *   import mostly.uncertaintee.syntax.chance.*
    *   import mostly.uncertaintee.syntax.coin.*
    *   import mostly.uncertaintee.syntax.collection.*
    *   import mostly.uncertaintee.syntax.comparison.*
    *   import mostly.uncertaintee.syntax.conversion.*
    *   import mostly.uncertaintee.syntax.dice.*
    *   import mostly.uncertaintee.syntax.distribution.*
    *   import mostly.uncertaintee.syntax.functional.*
    *   import mostly.uncertaintee.syntax.option.*
    *   import mostly.uncertaintee.syntax.quantile.*
    *   import mostly.uncertaintee.syntax.statistical.*
    * }}}
    */
  object syntax extends ops.AllOps {
    val all: ops.AllOps                          = this
    val arithmetic: ops.ArithmeticOps            = this
    val boolean: ops.BooleanOps                  = this
    val chance: ops.ChanceOps                    = this
    val coin: ops.CoinFlipOps                    = this
    val collection: ops.CollectionOps            = this
    val comparison: ops.ComparisonOps            = this
    val conversion: ops.ConversionOps            = this
    val dice: ops.DiceRollingOps                 = this
    val distribution: AllDistributionOps         = this
    val functional: ops.FunctionalProgrammingOps = this
    val option: ops.OptionOps                    = this
    val quantile: ops.QuantileOps                = this
    val statistical: ops.StatisticalOps          = this
  }

  // To avoid any funky accidents where I mistype numbers or round floats or something
  // incorrect; locking these down as constants. Not exposed to end user of library.
  private[uncertaintee] val Zero: Double     = Numeric[Double].zero
  private[uncertaintee] val One: Double      = Numeric[Double].one
  private[uncertaintee] val Two: Double      = Numeric[Double].fromInt(2)
  private[uncertaintee] val MinusTwo: Double = Numeric[Double].fromInt(-2)
}
