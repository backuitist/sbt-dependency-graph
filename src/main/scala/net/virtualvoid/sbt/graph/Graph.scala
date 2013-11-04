/* sbt -- Simple Build Tool
 * Copyright 2011 Mark Harrah, Eugene Yokota
 *
 * Copied from sbt 0.12 source code
 */
package net.virtualvoid.sbt.graph

import sbt.SbtDependencyGraphCompat

object Graph {

  /**
   * Formats a tree as follow:
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

    val twoSpaces = " " + " " // prevent accidentally being converted into a tab

    def limitLine(s: String): String = {
      if (s.length > maxColumn) s.slice(0, maxColumn - 2) + ".."
      else s
    }

    def toAsciiLines(nodes: List[A], prefix: String) : Seq[String] = {
      nodes match {
        case Nil => Seq()
        case node :: Nil =>
          limitLine(prefix + "+-" + display(node)) +:
            toAsciiLines(children(node).toList, prefix + twoSpaces) :+
            prefix

        case node :: remain =>
          limitLine(prefix + "+-" + display(node)) +:
            (toAsciiLines(children(node).toList, prefix + "| ") ++
          toAsciiLines(remain, prefix) )
      }
    }

    (display(top) +: toAsciiLines(children(top).toList, twoSpaces)).mkString("\n")
  }

  def defaultColumnSize: Int = {
    val termWidth = SbtDependencyGraphCompat.getTerminalWidth
    if (termWidth > 20) termWidth - 8
    else 80 // ignore termWidth
  }
}
