/* sbt -- Simple Build Tool
 * Copyright 2011 Mark Harrah, Eugene Yokota
 *
 * Copied from sbt 0.12 source code
 */
package net.virtualvoid.sbt.graph

import sbt.SbtDependencyGraphCompat
import scala.collection.mutable.ArrayBuffer
import scala.collection

object Graph {

  val twoSpaces = " " + " " // prevent accidentally being converted into a tab

  /**
   * Format a tree into a String, cutting out cycles.
   *
   * Format is as follow:
   * {{{
   * foo
   *   +-bar
   *   | +-baz1
   *   | | +-baz1.1
   *   | |
   *   | +-baz2
   *   | +-baz3
   *   |
   *   +-quux
   *     +-last
   * }}}
   */
  def toAscii[A](top: A,
                 children: A => Seq[A],
                 display: A => String,
                 maxColumn: Int = defaultColumnSize): String = {

    val executed = new collection.mutable.HashSet[A]()

    def limitLine(s: String): String = {
      if (s.length > maxColumn) s.slice(0, maxColumn - 2) + ".."
      else s
    }

    sealed trait GraphNode {
      def valueOption : Option[A]
      def isLast : Boolean
      def prefix: String
    }
    case object Root extends GraphNode {
      val valueOption = Some(top)
      val isLast = true
      val prefix = ""
      override def toString = display(top)
    }
    case class Separator(prefix: String) extends GraphNode {
      val valueOption = None
      val isLast = true
      override def toString = prefix
    }
    case class Element(value: A, prefix: String, isLast: Boolean) extends GraphNode {
      val valueOption = Some(value)
      override def toString : String = {
        val sep = if( executed.contains(value) ) "#-" else "+-"
        limitLine(prefix + sep + display(value))
      }
    }

    val lines = new ArrayBuffer[String]()

    val nodesToDisplay = new collection.mutable.Stack[GraphNode]()

    nodesToDisplay.push(Root)

    while(!nodesToDisplay.isEmpty) {
      val node = nodesToDisplay.pop()
      lines.append(node.toString)

      for( value <- node.valueOption ) {
        if( !executed.contains(value) ) {
          val childPrefix = node.prefix + (if( node.isLast ) twoSpaces else "| ")
          val currentChildren = children(value).map( c => Element(c, childPrefix, false)).reverse.toList match {
            case Element(value,prefix,_) :: tail => Element(value,prefix, true) :: tail
            case Nil => Nil
          }
          if( !nodesToDisplay.isEmpty && (!nodesToDisplay.top.isInstanceOf[Separator]) && !currentChildren.isEmpty ) {
            nodesToDisplay.push(Separator(childPrefix.dropRight(1)))
          }
          nodesToDisplay.pushAll(currentChildren)
          executed.add(value)
        }
      }
    }

    lines.mkString("\n")
  }

  def defaultColumnSize: Int = {
    val termWidth = SbtDependencyGraphCompat.getTerminalWidth
    if (termWidth > 20) termWidth - 8
    else 80 // ignore termWidth
  }
}
