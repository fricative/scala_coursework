package objsets

import common._
import TweetReader._
import java.util.NoSuchElementException;

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet

  def isEmpty: Boolean

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList = Nil

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  def getstring(): String

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

}

class Empty extends TweetSet {

  def getstring = "{N/A}"

  def union(that: TweetSet): TweetSet = {
    if (that.isEmpty) new Empty
    else that
  }

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def filter(p: Tweet => Boolean) = new Empty

  def isEmpty = true;

  def mostRetweeted: Tweet = {
    throw new java.util.NoSuchElementException;
  }

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def getstring(): String = {
    " { " + this.left.getstring + " , " + elem.user + " , " + this.right.getstring + "}"
  }

  def union(that: TweetSet): TweetSet = {
    if (that.isEmpty) this
    else {
      if (!that.contains(this.elem))
        this.right union (this.left union (that.incl(this.elem)))
      else
        this.right union this.left union that
    }
  }

  def isEmpty = false;

  def filter(p: Tweet => Boolean): TweetSet = {
    filterAcc(p, new Empty)
  }

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if (p(this.elem))
      (this.remove(elem)).filterAcc(p, acc incl (this.elem))
    else (this.remove(elem)).filterAcc(p, acc)
  }

  override def descendingByRetweet: TweetList = {
    if (this.left.isEmpty && this.right.isEmpty) new Cons(this.elem, Nil)
    else new Cons(this.elem, this.remove(this.elem).descendingByRetweet)
  }

  def mostRetweeted: Tweet = {
    this.descendingByRetweet.head
  }
  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val all = TweetReader.allTweets

  lazy val googleTweets: TweetSet = {
    if (all.isEmpty) new Empty
    else
      google(all.descendingByRetweet, new Empty)
  }

  lazy val appleTweets: TweetSet = {
    if (all.isEmpty) new Empty
    else
      apple(all.descendingByRetweet, new Empty)
  }

  def google(list: TweetList, set1: TweetSet): TweetSet = {
    if (list.isEmpty) set1
    else {
      if (help(google, list.head.text)) google(list.tail, set1.incl(list.head))
      else google(list.tail, set1)
    }
  }

  def apple(list: TweetList, set: TweetSet): TweetSet = {
    if (list.isEmpty) set
    else {
      if (help(apple, list.head.text)) apple(list.tail, set.incl(list.head))
      else apple(list.tail, set)
    }
  }

  def help(list: List[String], s: String): Boolean = {
    if (list.isEmpty) false
    else {
      if (s.contains(list(0))) true
      else help(list.tail, s)
    }
  }

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = {
    if (all.isEmpty) Nil
    else {
      trends(all.descendingByRetweet, Nil)
    }
  }

  def trends(list: TweetList, list2: TweetList): Cons = {
    if (list.isEmpty) list2
    if (help(google, list.head.text) || help(apple, list.head.text))
      trends(list.tail, inserter(list.head, list2))
    else
      trends(list.tail, list2)
  }

  def inserter(tw: Tweet, list: TweetList): TweetList = {
    if (list.isEmpty) new Cons(tw, Nil)
    else {
      if (tw.retweets > list.head.retweets) new Cons(tw, list)
      else {
        new Cons(list.head, inserter(tw, list.tail))
      }
    }
  }

}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
