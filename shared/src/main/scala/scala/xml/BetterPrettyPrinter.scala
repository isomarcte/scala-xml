package scala.xml

import scala.annotation._
import scala.collection.mutable.StringBuilder

final class BetterPrettyPrinter(width: Int, step: Int, minimizeEmpty: Boolean) extends PrettyPrinter(width, step, minimizeEmpty){
  import BetterPrettyPrinter._

  private def safeInt(i: Int): Int = scala.math.max(i, 1)
  private val safeWidth: Int = safeInt(width)
  private val safeStep: Int = safeInt(step)

  override final def format(n: Node, pscope: NamespaceBinding = TopScope): String = ???
  override final def format(n: Node, pscope: NamespaceBinding, sb: StringBuilder): Unit = ???
  override final def formatNodes(nodes: scala.collection.Seq[scala.xml.Node], pscope: scala.xml.NamespaceBinding, sb: StringBuilder): Unit = ???
  override final def formatNodes(nodes: scala.collection.Seq[scala.xml.Node], pscope: scala.xml.NamespaceBinding): String =
    ???

  @tailrec
  private[this] def loop(context: Context)(nodes: Seq[Node]): String = 
    if (nodes.isEmpty) {
      context.sb.toString
    } else {
      val node: Node = nodes.head
      loop(context)(nodes)
    }

  private def nodePreamble(context: Context)(node: Node): Context = {
    val sb: StringBuilder = context.sb
    node match {
      case node: SpecialNode =>
        node match {
          case node: Atom[_] =>
            context.copy(sb = sb.append(node.text.trim))
          case node: Comment =>
            context.copy(sb = sb.append(
            }
        }
    }
}

object BetterPrettyPrinter {
  private final case class Context(sb: StringBuilder, currentIndentationLevel: Int, cursor: Int)

  private object Context {

    def initialContext: Context = 
      Context(new StringBuilder(), 0, 0)
    // def heuristicInitCapacity(
    //   nodes: Seq[Node],
    //   width: Int,
    //   step: Int
    // ): Int = {
    //   val size: Int = nodes.size
    //   1
    // }
  }
}
