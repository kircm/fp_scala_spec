package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
    * how often the character appears.
    * This list is sorted alphabetically w.r.t. to the character in each pair.
    * All characters in the occurrence list are lowercase.
    *
    * Any list of pairs of lowercase characters and their frequency which is not sorted
    * is **not** an occurrence list.
    *
    * Note: If the frequency of some character is zero, then that character should not be
    * in the list.
    */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
    * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
    */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
    *
    * Note: the uppercase and lowercase version of the character are treated as the
    * same character, and are represented as a lowercase character in the occurrence list.
    *
    * Note: you must use `groupBy` to implement this method!
    */
  def wordOccurrences(w: Word): Occurrences = {
    val grouped: List[(Char, Int)] = w.groupBy(c => c.toLower).map { case (k, v) => (k, v.length) }.toList
    val sorted: List[(Char, Int)] = grouped.sortBy { case (c, i) => c }
    sorted
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    val allWordsTogether = s.fold("")((w1, w2) => w1 + w2)
    wordOccurrences(allWordsTogether)
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
    * the words that have that occurrence count.
    * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
    *
    * For example, the word "eat" has the following character occurrence list:
    *
    * `List(('a', 1), ('e', 1), ('t', 1))`
    *
    * Incidentally, so do the words "ate" and "tea".
    *
    * This means that the `dictionaryByOccurrences` map will contain an entry:
    *
    * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    *
    */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.foldLeft(Map[Occurrences, List[Word]]())((m, w) => addToMap(m, w))
  }

  def addToMap(m: Map[Occurrences, List[Word]], w: Word): Map[Occurrences, List[Word]] = {
    val occ: Occurrences = wordOccurrences(w)
    if (!m.contains(occ)) {
      m + (occ -> List(w))
    } else {
      val newList = m(occ) :+ w
      m + (occ -> newList)
    }
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences(wordOccurrences(word))
  }

  /** Returns the list of all subsets of the occurrence list.
    * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
    * is a subset of `List(('k', 1), ('o', 1))`.
    * It also include the empty subset `List()`.
    *
    * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
    *
    * List(
    * List(),
    * List(('a', 1)),
    * List(('a', 2)),
    * List(('b', 1)),
    * List(('a', 1), ('b', 1)),
    * List(('a', 2), ('b', 1)),
    * List(('b', 2)),
    * List(('a', 1), ('b', 2)),
    * List(('a', 2), ('b', 2))
    * )
    *
    * Note that the order of the occurrence list subsets does not matter -- the subsets
    * in the example above could have been displayed in some other order.
    */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case List() => List(List())
    case occ :: occs => {
      //IN:   List((a, 2), (b, 2))
      val allCharOccurrencesExpanded = expandAllCharOccurrences(occurrences)
      //OUT:  List(List((a,1)), List((a,2)), List((b,1)), List((b,2)))

      //IN:   List(List((a,1)), List((a,2)), List((b,1)), List((b,2)))
      val allCharPairs = allCharOccurrencesExpanded.flatten
      //OUT:  List((a,1), (a,2), (b,1), (b,2))

      //IN:  List((a,1), (a,2), (b,1), (b,2))
      val allPairsCombined = combinationsAcc(allCharPairs, List())
      //OUT:  List(List((a,1), (b,2)),
      //           List((a,1), (b,1)),
      //           List((a,2), (b,2)),
      //           List((a,2), (b,1)))

      //IN (new example):  List((a,1), (b,1), (b,2), (c,1))
      val allNCombined = combinationsAccRec(allCharPairs, List())

      // TODO: Combinations of 3 and bigger number of pairs
      val picks = pickAll(allCharPairs, List(), 3)
      println("OUT")
      println(picks)
      //OUT:               List(List((a,1), (b,1), (c,1)),
      //                        List((a,1), (b,2), (b,1)))

      val all = allPairsCombined ++ allCharOccurrencesExpanded ++ allNCombined ++ List(List())
      all
    }
  }

  // TODO - WIP
  def pickAll(rem: List[(Char, Int)], acc: List[List[(Char, Int)]], n: Int): List[List[(Char, Int)]] = rem match {
    case List() => acc
    case p :: rest => List(pickNHelper(p, rem, List(), n)) ++ pickAll(rem.tail, List(), n)
  }

  // TODO - WIP
  def pickNHelper(cur: (Char, Int), rem: List[(Char, Int)], acc: List[(Char, Int)], n: Int): List[(Char, Int)] = {
    if (acc.size == n) return acc

    rem match {
      case List() => List()
      case p :: rest => {
        if (!containsPair(acc, p._1)) {
          pickNHelper(cur, rem.tail, acc :+ p, n)
        } else {
          pickNHelper(cur, rem.tail, acc, n)
        }
      }
    }
  }

  def containsPair(listPairs: List[(Char, Int)], char: Char): Boolean = {
    !listPairs.filter { case (c, i) => c.equals(char) }.isEmpty
  }

  def combinationsAccRec(allCharPairs: List[(Char, Int)], combAcc: List[List[(Char, Int)]]): List[List[(Char, Int)]] = {
    println(allCharPairs.size)
    if (allCharPairs.size < 3) return combAcc

    if (allCharPairs.size == 3) {
      if (allNDifferent(allCharPairs, 3)) {
        List(List(allCharPairs.head, allCharPairs.tail.head, allCharPairs.tail.tail.head))
      } else {
        combAcc
      }
    } else {
      combinationsAccRec(allCharPairs.take(3), combAcc) ++ combinationsAccRec(allCharPairs.tail, combAcc)
    }
  }

  def allNDifferent(listPairs: List[(Char, Int)], n: Int): Boolean = {
    val chars: Set[Char] = listPairs.foldLeft(Set[Char]())((s, p) => s + p._1)
    chars.size == n
  }

  def combinationsAcc(allCharPairs: List[(Char, Int)], combAcc: List[List[(Char, Int)]]): List[List[(Char, Int)]] = allCharPairs match {
    case List() => combAcc
    case occ :: List() => combAcc
    case p1 :: p2 :: pRest => {
      val comb1 = combinationsAcc(p1 :: pRest, combAcc)
      val comb2 = combinationsAcc(p2 :: pRest, combAcc)
      if (!p1._1.equals(p2._1)) (comb1 ++ comb2) :+ List(p1, p2)
      else (comb1 ++ comb2)
    }
  }

  def combinationsForThisCharOccurrances(occ: (Char, Int)): List[Occurrences] = {
    val thisCharOcc: List[(Char, Int)] = (for (i <- 1 to occ._2) yield (occ._1, i)).toList
    val thisCharOccExp = for (elem <- thisCharOcc) yield List(elem)
    thisCharOccExp
  }

  def expandAllCharOccurrences(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case List() => Nil
    case occ :: occs => {
      val thisCharComb = combinationsForThisCharOccurrances(occ)
      thisCharComb ++ expandAllCharOccurrences(occs)
    }
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
    *
    * The precondition is that the occurrence list `y` is a subset of
    * the occurrence list `x` -- any character appearing in `y` must
    * appear in `x`, and its frequency in `y` must be smaller or equal
    * than its frequency in `x`.
    *
    * Note: the resulting value is an occurrence - meaning it is sorted
    * and has no zero-entries.
    */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xExp = expandAllCharOccurrences(x).flatten
    val yExp = expandAllCharOccurrences(y).flatten
    val xSubs = xExp.filterNot({ case (c, i) => yExp.contains((c, i)) })
    val xChars = for (x <- xSubs) yield x._1
    val xOccurs = xChars.foldLeft(Map[Char, Int]() withDefaultValue(0)){(occMap, c) => occMap.updated(c, occMap(c) + 1)}
    xOccurs.toList
  }

  /** Returns a list of all anagram sentences of the given sentence.
    *
    * An anagram of a sentence is formed by taking the occurrences of all the characters of
    * all the words in the sentence, and producing all possible combinations of words with those characters,
    * such that the words have to be from the dictionary.
    *
    * The number of words in the sentence and its anagrams does not have to correspond.
    * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
    *
    * Also, two sentences with the same words but in a different order are considered two different anagrams.
    * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
    * `List("I", "love", "you")`.
    *
    * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
    *
    * List(
    * List(en, as, my),
    * List(en, my, as),
    * List(man, yes),
    * List(men, say),
    * List(as, en, my),
    * List(as, my, en),
    * List(sane, my),
    * List(Sean, my),
    * List(my, en, as),
    * List(my, as, en),
    * List(my, sane),
    * List(my, Sean),
    * List(say, men),
    * List(yes, man)
    * )
    *
    * The different sentences do not have to be output in the order shown above - any order is fine as long as
    * all the anagrams are there. Every returned word has to exist in the dictionary.
    *
    * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
    * so it has to be returned in this list.
    *
    * Note: There is only one anagram of an empty sentence.
    */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = sentence match {
    case List() => List(List())
    case w :: wrds => List(List())
  }

//  def sentenceAnagramsAcc(sentence: Sentence, acc: Sentence): List[Sentence] = sentence match {
//
//  }

}
