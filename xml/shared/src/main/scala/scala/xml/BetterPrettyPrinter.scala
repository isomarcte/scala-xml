package scala.xml

import scala.annotation._
import scala.collection.mutable.StringBuilder

// Fixes
// * No trailing space
// * Break inside tag, between attributes
// * Properly escape ' inside attribute values
final class BetterPrettyPrinter(width: Int, step: Int, minimizeEmpty: Boolean) extends PrettyPrinter(width, step, minimizeEmpty){

  private def safeInt(i: Int): Int = scala.math.max(i, 1)
  private val safeWidth: Int = safeInt(width)
  private val safeStep: Int = safeInt(step)

  override final def format(n: Node, pscope: NamespaceBinding = TopScope): String = ???
  override final def format(n: Node, pscope: NamespaceBinding, sb: StringBuilder): Unit = ???
  override final def formatNodes(nodes: scala.collection.Seq[scala.xml.Node], pscope: scala.xml.NamespaceBinding, sb: StringBuilder): Unit = ???
  override final def formatNodes(nodes: scala.collection.Seq[scala.xml.Node], pscope: scala.xml.NamespaceBinding): String =
    ???
}

object BetterPrettyPrinter {

  private[this] def headTailV[A](value: Vector[A]): Option[(A, Vector[A])] =
    value.headOption.map(h =>
      (h, value.tail)
    )

  private def guardNullOrEmpty(value: String): Option[String] =
    Option(value).flatMap{
      case "" => None
      case otherwise => Some(otherwise)
    }

  private def emptyIfNull(value: String): String =
    Option(value).getOrElse("")

  private[this] type Level = Int

  private[this] sealed trait LastAction extends Product with Serializable

  private[this] object LastAction {
    case object EmitText extends LastAction
    case object EmitSpace extends LastAction
    case object EmitBreak extends LastAction
  }

  private[this] sealed trait OpType extends Product with Serializable

  private[this] object OpType {
    case object Open extends OpType
    case object Close extends OpType
  }

  private[this] final case class Op(node: Node, opType: OpType)

  private[this] object Op {
    def fromNode(node: Node, minimizeEmpty: Boolean): Vector[Op] =
      if (minimizeEmpty && node.child.isEmpty) {
        Vector(Op(node, OpType.Open))
      } else {
        Vector(Op(node, OpType.Open), Op(node, OpType.Close))
      }

    def fromNodeSeq(nodeSeq: NodeSeq, minimizeEmpty: Boolean): Vector[Op] =
      nodeSeq.foldLeft(Vector.empty[Op]){
        case (acc, value) =>
          acc ++ fromNode(value, minimizeEmpty)
      }
  }

  private[this] final case class CursorConfig private(
    width: Int,
    step: Int,
    minimizeEmpty: Boolean,
    pscope: NamespaceBinding,
    sb: StringBuilder
  )

  private[this] object CursorConfig {
    def newConfig(
      width: Int,
      step: Int,
      minimizeEmpty: Boolean,
      pscope: NamespaceBinding,
      optSb: Option[StringBuilder]
    ): CursorConfig =
      CursorConfig(
        scala.math.max(0, width),
        scala.math.max(0, step),
        minimizeEmpty,
        pscope,
        optSb.getOrElse(new StringBuilder())
      )
  }

  private[this] final case class Cursor private(cursorConfig: CursorConfig, emittedCharsOnCurrentLine: Int, level: Level, lastAction: LastAction) {
    private val availableChars: Int =
      cursorConfig.width - (level * cursorConfig.step) - emittedCharsOnCurrentLine

    private def emitString(value: String): Cursor = {
      println(s"${availableChars}, ${value.size}, ${value}")
      guardNullOrEmpty(value).fold(this){value =>
        val size: Int = value.size
        if(lastAction == LastAction.EmitBreak || size < availableChars) {
          cursorConfig.sb.append(value)
          this.copy(
            lastAction = LastAction.EmitText,
            emittedCharsOnCurrentLine = emittedCharsOnCurrentLine + size
          )
        } else {
          emitBreak.emitString(value)
        }
      }
    }

    private def emitSpace: Cursor = {
      if(lastAction == LastAction.EmitBreak || availableChars > 0) {
        cursorConfig.sb.append(" ")
        this.copy(
          lastAction = LastAction.EmitSpace, emittedCharsOnCurrentLine = emittedCharsOnCurrentLine + 1
        )
      } else {
        emitBreak
      }
    }

    private def emitBreak: Cursor = {
      val indentSize: Int = level * cursorConfig.step
      val indent: String =
        new String(Array.fill(indentSize)(32.toByte))
      cursorConfig.sb.append(s"\n${indent}")
      this.copy(lastAction = LastAction.EmitBreak, emittedCharsOnCurrentLine = indentSize)
    }

    private def increaseLevel: Cursor =
      this.copy(
        level = level + 1
      )

    private def decreaseLevel: Cursor =
      this.copy(
        level = level - 1
      )

    private def emitMetaData(metaData: MetaData): Cursor =
      Option(metaData).flatMap(metaData =>
        if (metaData.isEmpty) {
          None
        } else {
          Some(metaData)
        }
      ).fold(this)(metaData =>
        metaData.foldLeft(emitSpace){
          case (cursor, metaData: Attribute) =>
            val attributePreamble: String =
              guardNullOrEmpty(metaData.pre).fold(
                emptyIfNull(metaData.key)
              )(pre => s"${pre}:${emptyIfNull(metaData.key)}")
            val escapedValues: String =
              metaData.value.mkString.replace("'", "&apos;")
            cursor.emitString(s"""${attributePreamble}='${escapedValues}'""").emitSpace
          case (cursor, metaData) =>
            // This is either `Null` or a user defined type. In the case of
            // the latter there is no way to know if things will work.
            cursor.emitMetaData(metaData)
        }
      )

    private def elemName(elem: Elem): String =
      guardNullOrEmpty(elem.prefix).fold(
        emptyIfNull(elem.label)
      )(prefix =>
        s"${prefix}:${(emptyIfNull(elem.label))}"
      )

    private def emitStartTagClose(noEndTag: Boolean): Cursor =
      if (noEndTag) {
        emitString("/>")
      } else {
        emitString(">").increaseLevel
      }

    private def emitNamespace(elem: Elem): Cursor = {
      val ns: String = elem.scope.buildString(cursorConfig.pscope).trim
      if (ns.nonEmpty) {
        emitSpace.emitString(ns)
      } else {
        this
      }
    }

    private def emitElemPreamble(elem: Elem): Cursor =
      emitString(s"<${elemName(elem)}").emitMetaData(elem.attributes).emitNamespace(elem).emitStartTagClose(
        cursorConfig.minimizeEmpty && elem.child.size < 1
      )

    private def emitPreamble(node: Node): Cursor =
      node match {
        case _: SpecialNode =>
          this.emitString(node.toString.trim)
        case node: Elem =>
          emitElemPreamble(node)
        case _ =>
          // This is either a Group, a Document, or someone created a new
          // class. We'll try to render via toString, this may or may not work
          // and may or may not blow up (in the case of Group, for example)
          this.emitString(node.toString.trim)
      }

    private def emitEpilogue(node: Node): Cursor =
      node match {
        case node: Elem =>
          this.decreaseLevel.emitString(s"</${elemName(node)}>")
        case _ =>
          // Nodes which do not have epilogues are no-ops
          this
      }

    def emit(op: Op): Cursor =
      op.opType match {
        case OpType.Open =>
          emitPreamble(op.node)
        case OpType.Close =>
          emitEpilogue(op.node)
      }
  }

  private object Cursor {
    def fromConfig(cursorConfig: CursorConfig): Cursor =
      Cursor(cursorConfig, 0, 0, LastAction.EmitBreak)
  }

  def formatNodes_(width: Int, step: Int, minimizeEmpty: Boolean)(
    nodeSeq: NodeSeq,
    pscope: NamespaceBinding = TopScope,
    sb: Option[StringBuilder] = None
  ): String = {

    @tailrec
    def loop(cursor: Cursor, ops: Vector[Op]): String = {
      headTailV(ops) match {
        case None =>
          cursor.cursorConfig.sb.toString
        case Some((Op(Group(nodes), OpType.Open), t)) =>
          // Deal with Group hack
          val flattenedGroup: Vector[Op] =
            nodes.flatMap(node => Op.fromNode(node, cursor.cursorConfig.minimizeEmpty)).toVector
          loop(
            cursor, flattenedGroup ++ t
          )
        case Some((op@Op(node, OpType.Open), t)) =>
          val childrenOps: Vector[Op] =
            node.child.flatMap(node => Op.fromNode(node, cursor.cursorConfig.minimizeEmpty)).toVector
          loop(
            cursor.emit(op), childrenOps ++ t
          )
        case Some((op, t)) =>
          loop(
            cursor.emit(op), t
          )
      }
    }

    loop(
      Cursor.fromConfig(CursorConfig.newConfig(width, step, minimizeEmpty, pscope, sb)),
      Op.fromNodeSeq(nodeSeq, minimizeEmpty),
    )
  }

  // def prettyPrintNode(width: Int, step: Int, minimizeEmpty: Boolean)(node: Node): String =
  //   prettyPrintNodeSeq(width, step, minimizeEmpty)(NodeSeq.fromSeq(Seq(node)))
}
