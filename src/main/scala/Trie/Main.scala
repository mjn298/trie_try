package Trie

object Main extends App {
  val b: Trie[String] = Branch[String](Empty, Empty, None)
  val bWithAdditions = b.insert(4, Some("foo")).insert(2, Some("bar")).insert(7, Some("baz"))
  println(Trie.merkleRoot(bWithAdditions).mkString(""))
  println(bWithAdditions)
}

