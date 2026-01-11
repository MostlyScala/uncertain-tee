package mostly.uncertaintee.ops

import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

/** probabilistic combinatorics
  *
  * {{{
  * import mostly.uncertaintee.syntax.boolean.*
  * // or just import all the syntax (recommended)
  * import mostly.uncertaintee.syntax.*
  * }}}
  */
trait CollectionOps {

  extension [T](head: Uncertain[T]) {

    /** 'cons' or 'construct' operator for prepending a T to a List[T] */
    def ::[B >: T](tail: Uncertain[List[T]]): Uncertain[List[B]] = for {
      head_ <- head
      tail_ <- tail
    } yield head_ :: tail_

    /** 'cons' or 'construct' operator for creating a new List from two T elements */
    def ::[B >: T](elem: Uncertain[B])(using DummyImplicit): Uncertain[List[B]] = for {
      head_ <- head
      tail_ <- elem
    } yield List(head_, tail_)
  }

  extension [T](lhs: Uncertain[List[T]]) {

    /** 'concat' or 'concatinate' operator to combine a LHS `Uncertain[List[T]]` and RHS `Uncertain[List[T]]` into one `Uncertain[List]` */
    def ++(rhs: Uncertain[List[T]]): Uncertain[List[T]] = for {
      heads <- lhs
      tails <- rhs
    } yield heads ++ tails
  }

  extension (uncertain: Uncertain.type) {

    /** All possible sub-ranges are equally likely. The resulting sub-range maintains the same step size as the input range.
      *
      * @param from
      *   The range to select a sub-range from.
      * @param random
      *   Random number generator to use.
      * @return
      *   An `Uncertain[Range]` representing a randomly bounded sub-range. The result can be empty if both points are the same.
      * @example
      *   {{{
      *   val r = Uncertain.subrange(from = 0 until 100)
      *   r.sample() // Could be Range(15, 87), Range(0, 100), Range(42, 42), etc.
      *
      *   val r2 = Uncertain.subrange(from = 0 until -100 by -1)
      *   r2.sample() // Could be Range(0, -50, -1), Range(-20, -80, -1), etc.
      *
      *   val r3 = Uncertain.subrange(from = 0 to 20 by 5)
      *   r3.sample() // Could be Range(5, 15, 5), Range(0, 20, 5), etc.
      *   }}}
      */
    def subrange(from: Range)(using random: Random = new Random()): Uncertain[Range] =
      if (from.isEmpty) {
        Uncertain.always(from)
      } else {
        for {
          // Choose two random indices into the range
          idx1      <- Uncertain.fromRange(0 to from.size)
          idx2      <- Uncertain.fromRange(0 to from.size)
          startIdx   = math.min(idx1, idx2)
          endIdx     = math.max(idx1, idx2)
          // Convert indices to actual values
          startValue = if (startIdx >= from.size) {
                         // At the boundary position (from.size), we use the range's endpoint value
                         from.end
                       } else {
                         from(startIdx)
                       }
          endValue   = if (endIdx >= from.size) {
                         from.end
                       } else {
                         from(endIdx)
                       }
        } yield Range(
          start = startValue,
          end = endValue,
          step = from.step
        )
      }

    // =============================
    // ===== of varying size =======
    // =============================

    /** Creates an uncertain slice of a list, where both the size of the slice and its starting position are chosen randomly.
      *
      * @param from
      *   The list to slice from.
      * @param ofSize
      *   A Range specifying the possible sizes for the slice (e.g., 3 to 5). Sizes must be >= 0 and <= collection size.
      * @param random
      *   Random number generator to use.
      * @tparam T
      *   The type of elements in the list.
      * @return
      *   An `Uncertain[List[T]]` representing a randomly selected slice. Returns `Uncertain.always(List.empty)` if the range is empty, or if the list is empty and the minimum
      *   required slice size is >= 1.
      * @example
      *   {{{
      *   val myList = List(10, 20, 30, 40, 50, 60) // Get a random slice of size 2, 3, or 4
      *   val randomSlice = Uncertain.slicesOfVaryingSize(ofSize = 2 to 4, from = myList)
      *   randomSlice.sample() // Could be List(20, 30), List(40, 50, 60), List(10, 20, 30, 40), etc.
      *   }}}
      */
    def slicesOfVaryingSize[T](ofSize: Range, from: List[T])(using random: Random = new Random()): Uncertain[List[T]] = {
      require(ofSize.last <= from.size, s"ofSize range's last element (${ofSize.last}) cannot exceed collection size (${from.size})")
      require(ofSize.start >= 0, s"ofSize range's start (${ofSize.start}) must be positive")
      for {
        size         <- Uncertain.fromRange(ofSize)
        maxStartIndex = from.size - size
        startIndex   <- Uncertain.fromRange(0 to maxStartIndex)
        endIndex      = startIndex + size
        segment       = from.slice(startIndex, endIndex)
      } yield segment
    }

    /** Creates a random subsequence that maintains the relative ordering of elements from the input list.
      *
      * Unlike `slices` which returns contiguous elements, this method can "skip" elements while preserving their original order. For example, from List(1,2,3,4,5), a subsequence
      * could be List(1,3,5) or List(2,4), but never List(5,3,4)
      *
      * All subsequences of a given size are equally likely.
      *
      * @param ofSize
      *   A Range specifying the possible lengths of the subsequence (e.g., 2 to 4). Sizes must be >= 0 and <= the collection size.
      * @param from
      *   The list to select a subsequence from.
      * @param random
      *   Random number generator to use.
      * @tparam T
      *   The type of elements in the list.
      * @return
      *   An `Uncertain[List[T]]` representing a randomly selected subsequence that maintains the original ordering.
      * @example
      *   {{{
      *   val items = List("A", "B", "C", "D", "E")
      *   val subseq = Uncertain.subsequences(ofSize = 2 to 3, from = items)
      *   subseq.sample() // Could be List("A", "C"), List("B", "D", "E"), List("A", "E"), etc.
      *   // Note: Will never produce List("C", "A") because that violates original ordering
      *
      *   // Compare with slices (contiguous):
      *   val slice = Uncertain.slices(ofSize = 2 to 3, from = items)
      *   slice.sample() // Could be List("B", "C"), List("D", "E"), List("A", "B", "C"), etc.
      *   // Slices are always contiguous runs from the original list
      *   }}}
      */
    def subsequencesOfVaryingSize[T](ofSize: Range, from: List[T])(using random: Random = new Random()): Uncertain[List[T]] = {
      require(ofSize.last <= from.size, s"ofSize range's last element (${ofSize.last}) cannot exceed collection size (${from.size})")
      require(ofSize.start >= 0, s"ofSize range's start (${ofSize.start}) must be positive")
      Uncertain
        .fromRange(ofSize)
        .flatMap(n =>
          rngTakeWithoutPutback(
            items = from,
            n = n,
            retainRelativeOrder = true
          )
        )
        .map(_.toList)
    }

    /** Creates an uncertain list representing a random combination (unordered selection without replacement) of elements from the input list.
      *
      * @param ofSize
      *   A Range specifying the possible sizes for the combination (e.g., 2 to 4). Sizes must be >= 0 and <= the collection size.
      * @param from
      *   The list to select from.
      * @param random
      *   Random number generator to use.
      * @tparam T
      *   The type of elements in the list.
      * @return
      *   An `Uncertain[List[T]]` representing a randomly selected combination. The order of elements in the result is random.
      *
      * @example
      *   {{{
      *   val items = List("A", "B", "C", "D")
      *   // Random combination of 2 or 3 elements
      *   val combo = Uncertain.combinationsOfVaryingSize(ofSize = 2 to 3, from = items)
      *   combo.sample() // Could be List("C", "A"), List("B", "D", "A"), etc.
      *
      *   // With duplicates in input
      *   val dupes = List("A", "A", "B", "C")
      *   val combo2 = Uncertain.combinationsOfVaryingSize(ofSize = 2 to 2, from = dupes)
      *   combo2.sample() // Could be List("A", "A"), List("A", "C"), List("C", "B")
      *   }}}
      */
    def combinationsOfVaryingSize[T](ofSize: Range, from: List[T])(using random: Random = new Random()): Uncertain[List[T]] = {
      require(ofSize.last <= from.size, s"ofSize range's last element (${ofSize.last}) cannot exceed collection size (${from.size})")
      require(ofSize.start >= 0, s"ofSize range's start (${ofSize.start}) must be positive")
      Uncertain
        .fromRange(ofSize)
        .flatMap { n =>
          rngTakeWithoutPutback(
            items = from,
            n = n
          )
        }
        .map(_.toList)
    }

    /** Creates an uncertain set representing a random subset of the input set. Each subset of the specified size is equally likely.
      *
      * @param ofSize
      *   A Range specifying the possible sizes for the subset (e.g., 3 to 5). Sizes must be >= 0 and <= collection size.
      * @param from
      *   The set to select from.
      * @param random
      *   Random number generator to use.
      * @tparam T
      *   The type of elements in the set.
      * @return
      *   An `Uncertain[Set[T]]` representing a randomly selected subset. The subset size is chosen uniformly from the specified range, and all subsets of that size are equally
      *   likely.
      * @example
      *   {{{
      *   val mySet = Set("A", "B", "C", "D", "E", "F")
      *   // Get a random subset of size 2 or 3
      *   val randomSubset = Uncertain.subsetsOfVaryingSize(ofSize = 2 to 3, from = mySet)
      *   randomSubset.sample() // Could be Set("A", "C"), Set("B", "D", "F"), etc.
      *
      *   // Fixed size subset
      *   val fixedSizeSubset = Uncertain.subsetsOfVaryingSize(ofSize = 4 to 4, from = mySet)
      *   fixedSizeSubset.sample() // Always returns a set with exactly 4 elements
      *   }}}
      */
    def subsetsOfVaryingSize[T](ofSize: Range, from: Set[T])(using random: Random = new Random()): Uncertain[Set[T]] = {
      require(ofSize.last <= from.size, s"ofSize range's last element (${ofSize.last}) cannot exceed collection size (${from.size})")
      require(ofSize.start >= 0, s"ofSize range's start (${ofSize.start}) must be positive")
      val items: List[T] = from.toList
      Uncertain.fromRange(ofSize).flatMap {
        case n if n == items.length => Uncertain.always(from) // for sets, ordering is irrelevant, so no need to randomize in this special case
        case n                      => rngTakeWithoutPutback(items, n).map(_.toSet)
      }
    }

    // ================================
    // =====  with fixed sizes  =======
    // ================================

    /** Creates an uncertain set representing a random subset of the input set with a fixed size.
      *
      * This is a convenience method that delegates to `subsetsOfVaryingSize` with a single-value range.
      *
      * @param size
      *   The exact size of the subset to generate. Must be >= 0 and <= collection size.
      * @param from
      *   The set to select from.
      * @param random
      *   Random number generator to use.
      * @tparam T
      *   The type of elements in the set.
      * @return
      *   An `Uncertain[Set[T]]` representing a randomly selected subset of the specified size.
      * @example
      *   {{{
      *   val mySet = Set("A", "B", "C", "D", "E")
      *   val fixedSubset = Uncertain.subsetsOfFixedSize(3, mySet)
      *   fixedSubset.sample() // Always returns a set with exactly 3 elements, e.g., Set("A", "C", "E")
      *   }}}
      */
    def subsetsOfFixedSize[T](size: Int, from: Set[T])(using random: Random = new Random()): Uncertain[Set[T]] = {
      require(size >= 0 && size <= from.size, s"size (was: ${size}) must be within size of the underlying collection (${from.size}")
      subsetsOfVaryingSize(
        ofSize = size to size,
        from = from
      )
    }

    /** Creates an uncertain list representing a random subsequence of the input list with a fixed size.
      *
      * This is a convenience method that delegates to `subsequencesOfVaryingSize` with a single-value range. Unlike slices which return contiguous elements, subsequences can
      * "skip" elements while preserving their original order.
      *
      * @param size
      *   The exact size of the subsequence to generate. Must be >= 0 and <= collection size.
      * @param from
      *   The list to select a subsequence from.
      * @tparam T
      *   The type of elements in the list.
      * @return
      *   An `Uncertain[List[T]]` representing a randomly selected subsequence of the specified size that maintains the original ordering.
      * @example
      *   {{{
      *   val items = List("A", "B", "C", "D", "E")
      *   val fixedSubseq = Uncertain.subsequencesOfFixedSize(3, items)
      *   fixedSubseq.sample() // Could be List("A", "C", "E"), List("B", "D", "E"), etc.
      *   // Note: Will never produce List("C", "A", "B") because that violates original ordering
      *   }}}
      */
    def subsequencesOfFixedSize[T](size: Int, from: List[T]): Uncertain[List[T]] = {
      require(size >= 0 && size <= from.size, s"size (was: ${size}) must be within size of the underlying collection (${from.size}")
      Uncertain.subsequencesOfVaryingSize(
        ofSize = size to size,
        from = from
      )
    }

    /** Creates an uncertain list representing a random slice of the input list with a fixed size.
      *
      * This is a convenience method that delegates to `slicesOfVaryingSize` with a single-value range. The slice maintains the original order of elements.
      *
      * @param size
      *   The exact size of the slice to generate. Must be >= 0 and <= collection size.
      * @param from
      *   The list to slice from.
      * @tparam T
      *   The type of elements in the list.
      * @return
      *   An `Uncertain[List[T]]` representing a randomly positioned slice of the specified size.
      * @example
      *   {{{
      *   val myList = List(10, 20, 30, 40, 50, 60)
      *   val fixedSlice = Uncertain.slicesOfFixedSize(3, myList)
      *   fixedSlice.sample() // Could be List(10, 20, 30), List(30, 40, 50), List(40, 50, 60), etc.
      *   }}}
      */
    def slicesOfFixedSize[T](size: Int, from: List[T]): Uncertain[List[T]] = {
      require(size >= 0 && size <= from.size, s"size (was: ${size}) must be within size of the underlying collection (${from.size}")
      Uncertain.slicesOfVaryingSize(
        ofSize = size to size,
        from = from
      )
    }

    /** Creates an uncertain list representing a random combination (unordered selection without replacement) of elements from the input list with a fixed size.
      *
      * This is a convenience method that delegates to `combinationsOfVaryingSize` with a single-value range. Elements are selected without replacement, and the order in the result
      * is not uniformly distributed.
      *
      * @param size
      *   The exact size of the combination to generate. Must be >= 0 and <= collection size.
      * @param from
      *   The list to select from.
      * @param random
      *   Random number generator to use.
      * @tparam T
      *   The type of elements in the list.
      * @return
      *   An `Uncertain[List[T]]` representing a randomly selected combination of the specified size.
      * @example
      *   {{{
      *   val items = List("A", "B", "C", "D", "E")
      *   val fixedCombo = Uncertain.combinationsOfFixedSize(3, items)
      *   fixedCombo.sample() // Could be List("B", "D", "A"), List("E", "C", "B"), etc.
      *   }}}
      */
    def combinationsOfFixedSize[T](size: Int, from: List[T])(using random: Random = new Random()): Uncertain[List[T]] = {
      require(size >= 0 && size <= from.size, s"size (was: ${size}) must be within size of the underlying collection (${from.size}")
      combinationsOfVaryingSize(
        ofSize = size to size,
        from = from
      )
    }

    // ==========================================
    // ======= "complete" operations ============
    // ==========================================

    /** Creates an uncertain list representing a random shuffling of the input list. Each permutation is equally likely.
      *
      * @param from
      *   The list to shuffle.
      * @param random
      *   Random number generator to use.
      * @tparam T
      *   The type of elements in the list.
      * @return
      *   An `Uncertain[List[T]]` representing a randomly shuffled version of the list.
      * @example
      *   {{{
      *   val myList = List(1, 2, 3, 4)
      *   val shuffles = Uncertain.permutations(myList)
      *   shuffles.sample() // Could be List(3, 1, 4, 2), List(2, 4, 1, 3), etc.
      *   }}}
      */
    def permutations[T](from: List[T])(using random: Random = new Random()): Uncertain[List[T]] =
      rngTakeWithoutPutback(from, from.length).map(_.toList)

    /** Creates an uncertain list representing a random slice of any size from the input list
      *
      * @param from
      *   The list to slice from.
      * @tparam T
      *   The type of elements in the list.
      * @return
      *   An `Uncertain[List[T]]` representing a randomly positioned slice of random size.
      * @example
      *   {{{
      *   val myList = List(1, 2, 3, 4)
      *   val anySlice = Uncertain.slices(myList)
      *   anySlice.sample() // Could be List(), List(2, 3), List(1, 2, 3, 4), List(4), etc.
      *   }}}
      */
    def slices[T](from: List[T])(using random: Random = new Random()): Uncertain[List[T]] = Uncertain.slicesOfVaryingSize(
      ofSize = 0 to from.size,
      from = from
    )

    /** Creates an uncertain set representing a random subset of any size from the input set.
      * @param from
      *   The set to select from.
      * @tparam T
      *   The type of elements in the set.
      * @return
      *   An `Uncertain[Set[T]]` representing a randomly selected subset of random size.
      * @example
      *   {{{
      *   val mySet = Set("X", "Y", "Z")
      *   val anySubset = Uncertain.subsets(mySet)
      *   anySubset.sample() // Could be Set(), Set("X", "Z"), Set("X", "Y", "Z"), Set("Y"), etc.
      *   }}}
      */
    def subsets[T](from: Set[T])(using random: Random = new Random()): Uncertain[Set[T]] = Uncertain.subsetsOfVaryingSize(
      ofSize = 0 to from.size,
      from = from
    )

    /** Creates an uncertain list representing a random subsequence of any size from the input list.
      *
      * This is a convenience method that generates subsequences of all possible sizes from 0 to the collection size. It delegates to `subsequencesOfVaryingSize` with a complete
      * range. Unlike slices which return contiguous elements, subsequences can "skip" elements while preserving their original order.
      *
      * @param from
      *   The list to select a subsequence from.
      * @param random
      *   Random number generator to use.
      * @tparam T
      *   The type of elements in the list.
      * @return
      *   An `Uncertain[List[T]]` representing a randomly selected subsequence of random size that maintains the original ordering.
      * @example
      *   {{{
      *   val items = List("A", "B", "C", "D")
      *   val anySubseq = Uncertain.subsequences(items)
      *   anySubseq.sample() // Could be List(), List("A", "C"), List("B", "C", "D"), List("A", "B", "C", "D"), etc.
      *   // Note: Will never produce List("C", "A") because that violates original ordering
      *   }}}
      */
    def subsequences[T](from: List[T])(using random: Random = new Random()): Uncertain[List[T]] =
      Uncertain.subsequencesOfVaryingSize(
        ofSize = 0 to from.size,
        from = from
      )

    /** Creates an uncertain list representing a random rotation of the input list.
      *
      * A rotation shifts all elements by a random number of positions, with elements that "fall off" the end wrapping around to the beginning. All possible rotations (including
      * the original list) are equally likely.
      *
      * @param from
      *   The list to rotate.
      * @param random
      *   Random number generator to use.
      * @tparam T
      *   The type of elements in the list.
      * @return
      *   An `Uncertain[List[T]]` representing a randomly rotated version of the list.
      * @example
      *   {{{
      *   val items = List("A", "B", "C", "D")
      *   val rotated = Uncertain.rotations(items)
      *   rotated.sample() // Could be List("A", "B", "C", "D"), List("B", "C", "D", "A"),
      *                    //          List("C", "D", "A", "B"), List("D", "A", "B", "C")
      *   }}}
      */
    def rotations[T](from: List[T])(using random: Random = new Random()): Uncertain[List[T]] =
      Uncertain.fromRange(from.indices).map(n => from.drop(n) ++ from.take(n))

    /** Creates an uncertain list representing a random combination of any size from the input list.
      *
      * This is a convenience method that generates combinations of all possible sizes from 0 to the collection size.
      *
      * @param from
      *   The list to select from.
      * @tparam T
      *   The type of elements in the list.
      * @return
      *   An `Uncertain[List[T]]` representing a randomly selected combination of random size.
      * @example
      *   {{{
      *   val items = List("A", "B", "C")
      *   val anyCombo = Uncertain.combinations(items)
      *   anyCombo.sample() // Could be List(), List("B", "A"), List("A", "B", "C"), List("C"), etc.
      *   }}}
      */
    def combinations[T](from: List[T])(using random: Random = new Random()): Uncertain[List[T]] = Uncertain.combinationsOfVaryingSize(
      ofSize = 0 to from.size,
      from = from
    )

    /** Interleave lists via an unbiased random merge. This algorithm can be conceptualized as shuffling together several "suits" of cards, where each input `List[T]` is one "suit"
      * of cards.
      *
      * @example
      *   {{{
      *     val spades   = List("J♠", "Q♠", "K♠")
      *     val hearts   = List("J♥", "Q♥", "K♥")
      *     val diamonds = List("J♦", "Q♦", "K♦")
      *     val clubs    = List("J♣", "Q♣", "K♣")
      *     // This will produce a 12-card list. The relative order within
      *     // each suit is always preserved. For example, "J♠" will
      *     // always appear before "Q♠" in the result.
      *     // A possible result (one of many thousands):
      *     //   List("J♠", "J♥", "J♦", "Q♠", "K♠", "J♣", "Q♥", "Q♦", "K♥", "K♦", "Q♣", "K♣")
      *     // An impossible result (violates order):
      *     //   List("Q♠", "J♠", ...)
      *    interleave(spades, hearts, diamonds, clubs).sample()
      *   }}}
      *
      * A naïve approach would be to simply grab a card randomly from one of the lists - but in cases where there's a discrepancy in size between lists, that would it likely to
      * "front load" the shorter lists' elements - that would be a "biased random merge". Instead, we do an unbiased merge - meaning that in the following scenario, A♠ is just as
      * likely to appear at the beginning as it is at the end of the resulting list.
      *
      * @example
      *   {{{
      *     val aceOfSpades = List("A♠")
      *     val allDiamonds = List("A♦", "2♦", "3♦", "4♦", "5♦", "6♦", "7♦","8♦", "9♦", "10♦", "J♦", "Q♦", "K♦")
      *
      *     // We are merging a 1-card list and a 13-card list.
      *     // There are 14 total cards, and 14 possible positions
      *     // for the "A♠" in the final list.
      *
      *     // Because this is an *unbiased* merge, the "A♠" has an
      *     // exactly 1/14 chance of ending up in *any* of the 14
      *     // possible positions.
      *
      *     // e.g., These outcomes are all equally likely:
      *     // List("A♠", "A♦", "2♦", ..., "K♦")  // 1/14 chance
      *     // List("A♦", "A♠", "2♦", ..., "K♦")  // 1/14 chance
      *     // List("A♦", "2♦", "A♠", ..., "K♦")  // 1/14 chance
      *     // ...
      *     // List("A♦", "2♦", ..., "K♦", "A♠")  // 1/14 chance*
      *     // The algorithm ensures this by:
      *     // 1. On the first step, total cards = 14.
      *     // 2. It picks a number from 0 to 13.
      *     // 3. The only way "A♠" is chosen is if the number is 0
      *     //    (probability 1/14).
      *     // 4. Any other number (1-13) will select "A♦"
      *     //    (probability 13/14).
      *     // 5. If "A♦" is picked, on the next step there are 13 cards
      *     //    left, and "A♠" has a 1/13 chance of being picked.
      *     // This proportional probability is what makes it unbiased.
      *     interleave(aceOfSpades, allDiamonds).sample()
      *   }}}
      * @param lists
      *   The variable-argument lists to interleave.
      * @param rand
      *   The random generator to use for shuffling.
      * @return
      *   An `Uncertain` computation that, when run, will produce a single, randomly interleaved list.
      */
    def interleave[T](lists: List[T]*)(using rand: Random = new Random()): Uncertain[List[T]] = {
      // This algorithm can be conceptualized as shuffling together several suits of
      // cards, where each list we're trying to merge together can be visualised as a suit of cards lying face-up on a table.
      type Card     = T
      type Suit     = List[Card] // a pile of cards lying face-up - for instance [all 13 ♣]
      type AllSuits = List[Suit] // [[all 13 ♦], [all 13 ♣], [all 13 ♥], [all 13 ♠]]
      type Deck     = ListBuffer[Card]

      // e.g. [[all 13 ♦], [all 13 ♣], [all 13 ♥], [all 13 ♠]]
      val unmergedSuits: AllSuits = lists.toList
      // e.g. 52
      val fullDeckSize: Int       = lists.map(_.size).sum

      Uncertain { () =>
        val resultingDeck: Deck = new ListBuffer[Card]()

        @tailrec
        def dealIntoResultingDeck(suits: AllSuits): Unit =
          if (suits.forall(_.isEmpty)) {
            () // done
          } else {
            val cardsRemaining  = fullDeckSize - resultingDeck.length
            // As long as we have non-empty suits to merge...
            // ...pick a random number representing one of the remaining cards
            val cardNumber: Int = rand.nextInt(cardsRemaining)

            // ...find which suit this random number "falls into"
            var cumulativeSize       = 0
            val chosenSuitIndex: Int = suits.indexWhere { suit =>
              cumulativeSize += suit.size
              cardNumber < cumulativeSize
            }

            val chosenSuit: Suit = suits(chosenSuitIndex) // e.g. pick hearts suit [J,Q,K,A] because card number fell within that

            // ...draw the first card from that chosen suit
            val selectedCardFromSuit: Card = chosenSuit.head // J
            val restOfTheCardsInSuit: Suit = chosenSuit.tail // [Q,K,A]

            // ...deal it to our deck
            val _ = resultingDeck += selectedCardFromSuit

            // ...and place the (now only containing [Q,K,A]) hearts-suit back on the table
            val suitsAfterRemovingCardFromSuit: AllSuits = suits.updated(
              index = chosenSuitIndex,
              elem = restOfTheCardsInSuit // [Q,K,A] overrides [J,Q,K,A]
            )
            // do it again
            dealIntoResultingDeck(suits = suitsAfterRemovingCardFromSuit)
          }

        dealIntoResultingDeck(unmergedSuits)
        // and back to a sane immutable world again
        resultingDeck.toList
      }
    }
  }

  // =========================================
  // =========  internal machinery  ==========
  // =========================================

  /** (Not to become part of public API) Takes n elements randomly without put-back, from items; n needs to be <= items.size) */
  private def rngTakeWithoutPutback[T](items: List[T], n: Int, retainRelativeOrder: Boolean = false)(using random: Random): Uncertain[Iterator[T]] = {
    require(n <= items.size && n >= 0, s"n (real value: $n) was not within range of items to take from (0, ${items.size}))")
    Uncertain { () =>
      n match {
        case 0                         => Iterator.empty
        case 1                         => Iterator(items(random.nextInt(items.size)))
        case n if n > items.length / 2 => random.shuffle(items).take(n).iterator
        case n                         =>
          val arrayOfShuffledIndexes: Array[Int] = items.indices.toArray
          // Partial Fisher-Yates: only shuffle the first N elements, more efficient for small N than shuffling an entire (potentially huge) list of items.
          for (originalIndex <- 0 until n) {
            val indexToSwapWith = originalIndex + random.nextInt(items.size - originalIndex)
            // Swap the values at originalIndex and indexToSwapWith
            val temp            = arrayOfShuffledIndexes(indexToSwapWith)
            arrayOfShuffledIndexes(indexToSwapWith) = arrayOfShuffledIndexes(originalIndex)
            arrayOfShuffledIndexes(originalIndex) = temp
          }
          // Take the first N (the shuffled indexes) and retrieve the corresponding items
          val indexes                            = arrayOfShuffledIndexes.iterator.take(n)
          if (retainRelativeOrder) {
            indexes.toList.sorted.map(randomizedIndex => items(randomizedIndex)).iterator
          } else {
            indexes.map(randomizedIndex => items(randomizedIndex))
          }
      }
    }
  }
}
