package AxoniTrie

sealed trait Trie {
  def value = Option[String]
  def insert(key: Int, value: String): Trie = {
    val bSequence = reverseBinary(key)
    traverseAndCreate(bSequence, value)
  }

  // Need to do DF traversal and update/insert
  def traverseAndCreate(bSeq: Seq[Char], value: String): Node = bSeq match {
    case x :: Nil =>  if (x == "0") /* update left */ else /* update right */
    case x::xs => traverseAndCreate(xs, value)
  }

//  def left: Option[Trie] = this match {
//    case node: Node => Some(node.leftTrie)
//    case _ : Leaf => None
//  }
//
//  def right: Option[Trie] = this match {
//    case node: Node => Some(node.rightTrie)
//    case _: Leaf => None
//  }
  def getVal(bSeq: Seq[Char]): Option[String] = bSeq match {
    case x::xs => if(x == 0) left.getVal(xs) else right.getVal(xs)
    case x :: Nil => if(x == 0) left.value else right.value
  }

  // Level Order I think
  def merkleRoot(): String = this match {
    case l: Leaf => l.value.get().hashCode().toString()
    case n: Node =>
  }

  def reverseBinary(i: Int): Seq[Char] = i.toBinaryString.reverse
}


case class Node(left: Node, right: Node, value: Option[String]) extends Trie
case class Leaf(value: Option[String]) extends Trie