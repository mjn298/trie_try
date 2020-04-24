package AxoniTrie

import scala.annotation.tailrec

sealed trait Trie[+A] {
  def insert[A](k: Int, s: Option[A]): Trie[A] = {
    val path = Trie.toBinary(k)

    def loop(p: Seq[Char], n: Trie[A]): Trie[A] = p match {
      case h :: t if h == '0' => Branch(loop(t, n.left), n.right, n.value)
      case h :: t if h == '1' => Branch(n.left, loop(t, n.right), n.value)
      case Nil => n match {
        case Leaf(_) => Leaf(s)
        case Branch(l, r, v) => Branch(l, r, s)
      }
    }

    loop(path, this)
  }

  def left: Trie[A] = this match {
    case b: Branch[A] => b.l
    case _ => Empty
  }

  def right: Trie[A] = this match {
    case b: Branch[A] => b.r
    case _ => Empty
  }

  def value: Option[A] = this match {
    case Branch(_, _, v) => v
    case Leaf(v) => v
    case Empty => None
  }

  def fold[A, B](z: B)(lf: Option[A] => B)(b: (B, B) => B): B = this match {
    case Leaf(v) => lf(v)
    case Branch(l, r, v: Option[A]) => b(l.fold(lf(v))(lf)(b), r.fold(lf(v))(lf)(b))
  }

  def getNode(key: Int): Option[Trie[A]] = {
    val path = Trie.toBinary(key)

    @tailrec
    def loop(p: Seq[Char], n: Option[Trie[A]]): Option[Trie[A]] = path match {
      case h :: t if h == '0' => loop(t, n map (t => t.left))
      case h :: t if h == '1' => loop(t, n map (t => t.right))
      case Nil => n
    }

    loop(path, Some(this))
  }

}

case class Leaf[A](v: Option[A]) extends Trie[A]

case class Branch[A](l: Trie[A], r: Trie[A], v: Option[A]) extends Trie[A]

case object Empty extends Trie[Nothing]

object Trie {
  def toBinary(i: Int): Seq[Char] = i.toBinaryString.reverse

  def hashVal(s: String): Int = s.hashCode
}