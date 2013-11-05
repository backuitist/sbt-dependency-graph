package net.virtualvoid.sbt.graph

import scala.util.Random
import org.junit.Test
import org.specs2.matcher.JUnitMustMatchers

class GraphTest extends JUnitMustMatchers {
  sealed trait Tree
  case class Branch(left: Tree, right: Tree) extends Tree
  case class Leaf(i: Int) extends Tree

  def children(t: Tree): Seq[Tree] = t match {
    case Branch(left, right) => Seq(left, right)
    case _: Leaf => Nil
  }
  def display(t: Tree): String = t match {
    case Branch(left, right) => "Branch"
    case Leaf(value) => value.toString * value
  }

  @Test
  def layoutSimpleGraphs() {
    val simple = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Graph.toAscii(simple, children, display, 20).trim must_==
      """Branch
        |  +-Branch
        |  | +-1
        |  | +-22
        |  |
        |  +-333
        |  """.stripMargin.trim
  }

  @Test
  def cutOffCycles() {
    Graph.toAscii[Int](1, Map(
      1 -> Seq(2,3,4),
      2 -> Seq(4,5),
      3 -> Seq(),
      4 -> Seq(3),
      5 -> Seq(1,4,6,7),
      6 -> Seq(),
      7 -> Seq()), _.toString).trim must_==
      """1
        |  +-2
        |  | +-4
        |  | | +-3
        |  | |
        |  | +-5
        |  |   #-1
        |  |   #-4
        |  |   +-6
        |  |   +-7
        |  |
        |  #-3
        |  #-4
      """.stripMargin.trim
  }

  @Test
  def notStackOverflow() {
    val bigGraph = (for( i <- 1 to 2000) yield i -> (1 to 20).map( i => Random.nextInt(3000)).toSeq).toMap
    Graph.toAscii[Int](1, i => bigGraph.getOrElse(i, Seq.empty[Int]), _.toString)
  }

  @Test
  def layoutDeepGraphs() {
    val simple = Branch(Branch(Branch(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Leaf(1)), Leaf(2)), Leaf(3)), Leaf(4))
    Graph.toAscii(simple, children, display, 10).trim must_==
      """Branch
        |  +-Branch
        |  | +-Br..
        |  | | +-..
        |  | | | ..
        |  | | | ..
        |  | | | ..
        |  | | | ..
        |  | | | | |
        |  | | | ..
        |  | | | |
        |  | | | ..
        |  | | |
        |  | | +-22
        |  | |
        |  | +-333
        |  |
        |  +-4444
        |  """.stripMargin.trim
  }
}
