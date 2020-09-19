package scala.xml.scalacheck

import java.util.regex.Pattern
import org.scalacheck._

trait Generators {

  def recursiveStep[A](value: Gen[A]): Gen[A] =
    Gen.sized(i =>
      Gen.resize(i - (i.toDouble/10D).toInt, value)
    )

  private def weight(size: Int, minSize: Int, totalSize: Int): Int = {
    val totalSizeBD: BigDecimal = BigDecimal(totalSize)
    ((BigDecimal(size) / totalSizeBD) * (BigDecimal(10) / (BigDecimal(minSize) / totalSizeBD))).toInt
  }

  /** A function to create an even distribution for a non empty list of int ranges.
    *
    * We use this because the `Gen.oneOf` construct by default gives equal
    * weight to all the `Gen` values. In the case where each `Gen` value
    * represents a range of values we want to give equal probability to
    * ''any'' value. This can be achieved with the `Gen.frequency` method, but
    * doing this manually for every range is a lot of error prone boilerplate.
    *
    * This function lets us perform this calculation automatically, at least
    * for integral ranges (including `char` which we are primarily interested
    * in). We can't reasonably use it in all cases, but it does help with some
    * common base cases.
    */
  private def evenDistribution(head: (Int, Int), tail: List[(Int, Int)]): Gen[Int] = {
    def rangeSize(bounds: (Int, Int)): Int = bounds match {
      case (lower, upper) => upper - lower + 1
    }

    val headSize: Int = rangeSize(head)
    val (totalSize, minSize, valuesWithSize) = tail.foldLeft((headSize, headSize, Nil: List[(Int, (Int, Int))])){
      case ((totalSize, minSize, acc), value) =>
        val valueSize: Int = rangeSize(value)
        ((totalSize + valueSize), scala.math.min(valueSize, minSize), (valueSize, value) +: acc)
    }

    Gen.frequency(
      valuesWithSize.foldLeft(
        List(weight(headSize, minSize, totalSize) -> Gen.choose(head._1, head._2))
      ){
        case (acc, (size, (lower, upper))) =>
          (weight(size, minSize, totalSize) -> Gen.choose(lower, upper)) +: acc
      }: _*
    )
  }

  lazy val xmlRestrictedChars: Set[Char] =
    (
      (0x1 to 0x8).toSet ++
        (0xB to 0xC).toSet ++
        (0xE to 0x1F).toSet ++
        (0x7F to 0x84).toSet ++
        (0x86 to 0x9F).toSet
    ).map(_.toChar)

  lazy val xmlChars: Set[Char] =
    ((0x1 to 0xD7FF).toSet ++
    (0xE000 to 0xFFFD).toSet ++
    (0x10000 to 0x10FFFF).toSet).map(_.toChar)

  /** Generate a valid XML `Char`.
    *
    * From the specification, "any Unicode character, excluding the surrogate blocks, FFFE, and FFFF.""
    *
    * Char ::= [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Char]]
    */
  lazy val xmlCharGen: Gen[Char] =
    Gen.oneOf(xmlChars)

  /** Generate a valid XML `RestrictedChar`.
    *
    * RestrictedChar ::= [#x1-#x8] | [#xB-#xC] | [#xE-#x1F] | [#x7F-#x84]
    *                  | [#x86-#x9F]
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-RestrictedChar]]
    */
  lazy val xmlRestrictedCharGen: Gen[Char] =
    Gen.oneOf(xmlRestrictedChars)

  /** Generate a valid XML White Space Character.
    *
    * S ::= (#x20 | #x9 | #xD | #xA)+
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-S]]
    */
  lazy val xmlWhiteSpaceCharGen: Gen[Char] =
    Gen.oneOf(0x20, 0x9, 0xD, 0xA).map(_.toChar)

  /** Generate a valid XML White Space Character.
    *
    * S ::= (#x20 | #x9 | #xD | #xA)+
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-S]]
    */
  lazy val xmlWhiteSpaceGen: Gen[String] =
    Gen.nonEmptyListOf(xmlWhiteSpaceCharGen).map(_.mkString)

  /** Generates a valid XML whitespace string or the empty string with a 50%
    * probability to each outcome.
    */
  lazy val xmlMaybeWhiteSpaceGen: Gen[String] =
    Gen.oneOf(Gen.const(""), xmlWhiteSpaceGen)

  /** Generate a valid `NameStartChar`.
    *
    * NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6]
    *                   | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] |
    *                   [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] |
    *                   [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] |
    *                   [#x10000-#xEFFFF]
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-NameStartChar]]
    */
  lazy val xmlNameStartCharGen: Gen[Char] =
    evenDistribution((':'.toInt, ':'.toInt), List(
      ('a'.toInt, 'z'.toInt),
      ('A'.toInt, 'Z'.toInt),
      ('_'.toInt, '_'.toInt),
      (0xC0, 0xD6),
      (0xD8, 0xF6),
      (0xF8, 0x2FF),
      (0x370, 0x37D),
      (0x37F, 0x1FFF),
      (0x200C, 0x200D),
      (0x2070, 0x218F),
      (0x2C00, 0x2FEF),
      (0x3001, 0xD7FF),
      (0xF900, 0xFDCF),
      (0xFDF0, 0xFFFD),
      (0x10000, 0xEFFFF)
    )).map(_.toChar)

  /** Generate a valid `NameChar`.
    *
    * NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F]
    *              | [#x203F-#x2040]
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-NameChar]]
    */
  lazy val xmlNameCharGen: Gen[Char] = {
    val totalSize: Int = 971506 + 125
    Gen.frequency(
      weight(971506, 125, totalSize) -> xmlNameStartCharGen,
      weight(125, 125, totalSize) -> evenDistribution(
        ('-'.toInt, '-'.toInt),
        List(('.'.toInt, '.'.toInt), ('0'.toInt, '9'.toInt), (0xB7, 0xB7), (0x0300, 0x036F))
      ).map(_.toChar)
    )
  }

  /** Generate a valid `Name`.
    *
    * Name ::= NameStartChar (NameChar)*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Name]]
    */
  lazy val xmlNameGen: Gen[String] = recursiveStep(
    for {
      nameStartChar <- xmlNameStartCharGen
      nameChars <- Gen.listOf(xmlNameCharGen).map(_.mkString)
    } yield nameStartChar.toString ++ nameChars
  )

  /** Generate a valid `Names`.
    *
    * Names ::= Name (#x20 Name)*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Names]]
    */
  lazy val xmlNamesGen: Gen[String] = recursiveStep(
    for {
      firstName <- xmlNameGen
      rest <- Gen.listOf(xmlNameGen.map(name => s" ${name}")).map(_.mkString)
    } yield firstName ++ rest
  )

  /** Generate a valid `Nmtoken`.
    *
    * Nmtoken ::= (NameChar)+
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Nmtoken]]
    */
  lazy val xmlNmtokenGen: Gen[String] = recursiveStep(
    Gen.nonEmptyListOf(xmlNameCharGen).map(_.mkString)
  )

  /** Generate a valid `Nmtokens`.
    *
    * Nmtokens ::= Nmtoken (#x20 Nmtoken)*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Nmtokens]]
    */
  lazy val xmlNmtokensGen: Gen[String] = recursiveStep(
    for {
      first <- xmlNmtokenGen
      rest <- Gen.listOf(xmlNmtokenGen.map(nmtoken => s" ${nmtoken}")).map(_.mkString)
    } yield first ++ rest
  )

  /** Generate a valid `Eq`.
    *
    * Eq ::= S? '=' S?
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Eq]]
    */
  lazy val xmlEqGen: Gen[String] = recursiveStep(
    for {
      prefixSpace <- Gen.oneOf("", " ")
      suffixSpace <- Gen.oneOf("", " ")
    } yield s"${prefixSpace}=${suffixSpace}"
  )

  /** Generate a valid `EntityRef`.
    *
    * EntityRef ::= '&' Name ';'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-EntityRef]]
    */
  lazy val xmlEntityRefStringGen: Gen[String] = recursiveStep(
    xmlNameGen.map(name => s"&${name};")
  )

  /** Generate a valid `CharRef`.
    *
    * CharRef ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-CharRef]]
    */
  lazy val xmlCharRefStringGen: Gen[String] = recursiveStep(
    Gen.oneOf(
      Gen.nonEmptyListOf(Gen.numChar).map(value =>
        s"&#${value.mkString};"
      ),
      Gen.nonEmptyListOf(
        evenDistribution(('0'.toInt, '9'.toInt), List(('a'.toInt, 'f'.toInt), ('A'.toInt, 'F'.toInt))).map(_.toChar)
      ).map(value =>
        s"&#x${value.mkString};"
      )
    )
  )

  /** Generate a valid `Reference`.
    *
    * Reference ::= EntityRef | CharRef
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Reference]]
    */
  lazy val xmlReferenceGen: Gen[String] = recursiveStep(
    Gen.oneOf(
      xmlEntityRefStringGen,
      xmlCharRefStringGen
    )
  )

  /** Generate a valid `Reference`.
    *
    * PEReference ::= '%' Name ';'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PEReference]]
    */
  lazy val xmlPEReferenceGen: Gen[String] = recursiveStep(
    xmlNameGen.map(value => s"%${value};")
  )

  /** Generate a valid `AttValue`.
    *
    * AttValue ::= '"' ([^<&"] | Reference)* '"'
    *            | "'" ([^<&'] | Reference)* "'"
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-AttValue]]
    */
  lazy val xmlAttValueGen: Gen[String] = recursiveStep{
    def common(delimiter: Char): Gen[String] = {
      val invalid: Set[Char] =
        Set(delimiter, '^', '<', '&')
      Gen.listOf(
        Gen.oneOf(
          Gen.oneOf(xmlChars -- invalid).map(_.toString),
          xmlReferenceGen
        )
      ).map(value => s"${delimiter}${value.mkString}${delimiter}")
    }
    Gen.oneOf(
      common('\''),
      common('"')
    )
  }

  /** Generate a valid `EntityValue`.
    *
    * EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
		*              |  "'" ([^%&'] | PEReference | Reference)* "'"
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-EntityValue]]
    */
  lazy val xmlEntityValueGen: Gen[String] = recursiveStep{
    def common(delimiter: Char): Gen[String] = {
      val invalid: Set[Char] =
        Set(delimiter, '^', '%', '&')
      Gen.listOf(
        Gen.oneOf(
          Gen.oneOf(xmlChars -- invalid).map(_.toString),
          xmlPEReferenceGen,
          xmlReferenceGen
        )
      ).map(value => s"${delimiter}${value}${delimiter}")
    }

    Gen.oneOf(
      common('\''),
      common('"')
    )
  }

  /** Generate a valid `SystemLiteral`
    *
    * SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-SystemLiteral]]
    */
  lazy val xmlSystemLiteralGen: Gen[String] = recursiveStep{
    def common(delimiter: Char): Gen[String] =
      Gen.listOf(Gen.oneOf(xmlChars -- Set(delimiter))).map(value =>
        s"${delimiter}${value.mkString}${delimiter}"
      )

    Gen.oneOf(
      common('\''),
      common('"')
    )
  }

  lazy val xmlPubidChars: Set[Char] =
    ((0x20 to 0x20).toSet ++
      (0xD to 0xD).toSet ++
      (0xA to 0xA).toSet ++
      ('a'.toInt to 'z'.toInt).toSet ++
      ('A'.toInt to 'Z'.toInt).toSet ++
      ('0'.toInt to '9'.toInt).toSet
    ).map(_.toChar) ++ Set('-', '\'', '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*', '#', '@', '$', '_', '%')

  /** Generate a valid `PubidChar`
    *
    * PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PubidChar]]
    */
  lazy val xmlPubidCharGen: Gen[Char] =
    Gen.oneOf(xmlPubidChars)

  /** Generate a valid `PubidLiteral`.
    *
    * PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PubidLiteral]]
    */
  lazy val xmlPubidLiteralGen: Gen[String] = recursiveStep(
    Gen.oneOf(
      Gen.listOf(xmlPubidCharGen).map(value => s""""${value.mkString}""""),
      Gen.listOf(Gen.oneOf(xmlPubidChars -- Set('\''))).map(value => s"'${value.mkString}'")
    )
  )

  /** Generate a valid `CharData`.
    *
    * CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-CharData]]
    */
  lazy val xmlCharDataGen: Gen[String] = recursiveStep{
    val invalid: Set[Char] =
      Set('^', '<', '&')
    Gen.listOf(Gen.oneOf(xmlChars -- invalid)).map(_.mkString).map(_.replace("]]>", "[[<"))
  }

  /** Generate a valid `Comment`.
    *
    * Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Comment]]
    */
  lazy val xmlCommentGen: Gen[String] = recursiveStep{
    val minusDash: Gen[String] =
      Gen.oneOf(xmlChars -- Set('-')).map(_.toString)
    Gen.listOf(
      Gen.oneOf(
        minusDash,
        minusDash.map(value => s"-${value}")
      )
    ).map(value => s"<!--${value.mkString}-->")
  }

  /** Generate a valid `PITarget`.
    *
    * PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PITarget]]
    */
  lazy val xmlPITargetGen: Gen[String] = {
    lazy val pattern: Pattern =
      Pattern.compile("""[xX][mM][lL]""")
    recursiveStep(
      xmlNameGen.map(value =>
        pattern.matcher(value).replaceAll("abc")
      )
    )
  }

  /** Generate a valid `PI`.
    *
    * PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PI]]
    */
  lazy val xmlPIGen: Gen[String] = recursiveStep{
    val restGen: Gen[String] =
      Gen.oneOf(
        for {
          s <- xmlWhiteSpaceGen
          c <- Gen.listOf(xmlCharGen).map(_.mkString).map(_.replace("?>", "ab"))
        } yield s"${s}${c}",
        Gen.const("")
      )
    for {
      piTarget <- xmlPITargetGen
      rest <- restGen
    } yield s"<?${piTarget}${rest}?>"
  }

  /** Generate a valid `CDStart`.
    *
    * CDStart ::= '<![CDATA['
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-CDStart]]
    */
  lazy val xmlCDStartGen: Gen[String] =
    Gen.const("<![CDATA[")

  /** Generate a valid `CDEnd`.
    *
    * CDEnd ::= ']]>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-CDEnd]]
    */
  lazy val xmlCDEndGen: Gen[String] =
    Gen.const("]]>")

  /** Generate a valid `CData`.
    *
    * CData ::= (Char* - (Char* ']]>' Char*))
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-CData]]
    */
  lazy val xmlCDataGen: Gen[String] = recursiveStep(
    Gen.listOf(xmlCharGen).map(_.mkString).map(_.replace("]]>", "abc"))
  )

  /** Generate a valid `CDSect`.
    *
    * CDSect ::= CDStart CData CDEnd
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-CDSect]]
    */
  lazy val xmlCDSectGen: Gen[String] = recursiveStep(
    for {
      cdstart <- xmlCDStartGen
      cdata <- xmlCDataGen
      cdend <- xmlCDEndGen
    } yield s"${cdstart}${cdata}${cdend}"
  )

  /** Generate a valid `VersionNum`.
    *
    * VersionNum ::= '1.1'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-VersionNum]]
    */
  lazy val xmlVersionNumGen: Gen[String] =
    Gen.const("1.1")

  /** Generate a valid `Misc`.
    *
    * Misc ::= Comment | PI | S
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Misc]]
    */
  lazy val xmlMiscGen: Gen[String] = recursiveStep(
    Gen.oneOf(
      xmlCommentGen,
      xmlPIGen,
      xmlWhiteSpaceGen
    )
  )

  /** Generate a valid `VersionInfo`.
    *
    * VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-VersionInfo]]
    */
  lazy val xmlVersionInfoGen: Gen[String] = recursiveStep{
    val restGen: Gen[String] =
      Gen.oneOf(
        xmlVersionNumGen.map(value => s"'${value}'"),
        xmlVersionNumGen.map(value => s""""${value}"""")
      )
    for {
      s <- xmlWhiteSpaceGen
      eq <- xmlEqGen
      rest <- restGen
    } yield s"${s}version${eq}${rest}"
  }

  /** Generate a valid `XMLDecl`.
    *
    * XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-XMLDecl]]
    */
  lazy val xmlXMLDeclGen: Gen[String] = recursiveStep(
    for {
      v <- xmlVersionInfoGen
      e <- Gen.oneOf(Gen.const(""), xmlEncodingDeclGen)
      sd <- Gen.oneOf(Gen.const(""), xmlSDDeclGen)
      s <- xmlMaybeWhiteSpaceGen
    } yield s"<?xml${v}${e}${sd}${s}?>"
  )

  /** Generate a valid `prolog`.
    *
    * prolog ::= XMLDecl Misc* (doctypedecl Misc*)?
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-prolog]]
    */
  lazy val xmlPrologGen: Gen[String] = recursiveStep{
    val misc: Gen[String] = Gen.listOf(xmlMiscGen).map(_.mkString)
    val epilogue: Gen[String] =
      for {
        d <- xmlDoctypedeclGen
        m <- misc
      } yield s"${d}${m}"
    for {
      x <- xmlXMLDeclGen
      m <- misc
      e <- Gen.oneOf(Gen.const(""), epilogue)
    } yield s"${x}${m}${e}"
  }

  /** Generate a valid `doctypedecl`.
    *
    * doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-doctypedecl]]
    */
  lazy val xmlDoctypedeclGen: Gen[String] = recursiveStep{
    val subGen0: Gen[String] =
      for {
        s <- xmlWhiteSpaceGen
        e <- xmlExternalIDGen
      } yield s"${s}${e}"

    val subGen1: Gen[String] =
      for {
        i <- xmlIntSubsetGen
        s <- xmlMaybeWhiteSpaceGen
      } yield s"[${i}]${s}"

    for {
      s0 <- xmlWhiteSpaceGen
      n <- xmlNameGen
      sub0 <- Gen.oneOf(Gen.const(""), subGen0)
      s1 <- xmlMaybeWhiteSpaceGen
      sub1 <- Gen.oneOf(Gen.const(""), subGen1)
    } yield s"<!DOCTYPE${s0}${n}${sub0}${s1}${sub1}>"
  }

  /** Generate a valid `DeclSep`.
    *
    * DeclSep ::= PEReference | S
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-DeclSep]]
    */
  lazy val xmlDeclSepGen: Gen[String] = recursiveStep(
    Gen.oneOf(
      xmlPEReferenceGen,
      xmlWhiteSpaceGen
    )
  )

  /** Generate a valid `intSubset`.
    *
    * intSubset ::= (markupdecl | DeclSep)*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-intSubset]]
    */
  lazy val xmlIntSubsetGen: Gen[String] = recursiveStep(
    Gen.listOf(
      Gen.oneOf(
        xmlMarkupDeclGen,
        xmlDeclSepGen
      )
    ).map(_.mkString)
  )

  /** Generate a valid `markupdecl`.
    *
    * markupdecl ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl |
    *                PI | Comment
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-markupdecl]]
    */
  lazy val xmlMarkupDeclGen: Gen[String] = recursiveStep(
    Gen.oneOf(
      xmlElementDeclGen,
      xmlAttlistDeclGen,
      xmlEntityDeclGen,
      xmlNotationDeclGen,
      xmlPIGen,
      xmlCommentGen
    )
  )

  /** Generate a valid `extSubset`.
    *
    * extSubset ::= TextDecl? extSubsetDecl
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-extSubset]]
    */
  lazy val xmlExtSubsetGen: Gen[String] = recursiveStep(
    for {
      t <- Gen.oneOf(Gen.const(""), xmlTextDeclGen)
      e <- xmlExtSubsetGen
    } yield s"${t}${e}"
  )

  /** Generate a valid `extSubsetDecl`.
    *
    * extSubsetDecl ::= ( markupdecl | conditionalSect | DeclSep)*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-extSubsetDecl]]
    */
  lazy val xmlExtSubsetDeclGen: Gen[String] = recursiveStep(
    Gen.listOf(
      Gen.oneOf(
        xmlMarkupDeclGen,
        xmlConditionalSectGen,
        xmlDeclSepGen
      )
    ).map(_.mkString)
  )

  /** Generate a valid `SDDecl`.
    *
    * SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-SDDecl]]
    */
  lazy val xmlSDDeclGen: Gen[String] = recursiveStep{
    val yesNoGen: Gen[String] =
      Gen.oneOf(
        Gen.const("yes"),
        Gen.const("no")
      )
    val restGen: Gen[String] =
      Gen.oneOf(
        yesNoGen.map(value => s""""${value}""""),
        yesNoGen.map(value => s"'${value}'"),
      )

    for {
      s <- xmlWhiteSpaceGen
      eq <- xmlEqGen
      rest <- restGen
    } yield s"${s}standalone${eq}${rest}"
  }

  /** Generate a valid `element`.
    *
    * element ::= EmptyElemTag | STag content ETag
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-element]]
    */
  lazy val xmlElementGen: Gen[String] = recursiveStep(
    Gen.oneOf(
      xmlEmptyElemTagGen,
      for {
        stag <- xmlSTagGen
        content <- xmlContentGen
        etag <- xmlETagGen
      } yield s"${stag}${content}${etag}"
    )
  )

  /** Generate a valid `STag`.
    *
    * STag ::= '<' Name (S Attribute)* S? '>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-STag]]
    */
  lazy val xmlSTagGen: Gen[String] = recursiveStep{
    val internalGen: Gen[String] =
      Gen.listOf(
        for {
          s <- xmlWhiteSpaceGen
          a <- xmlAttributeGen
        } yield s"${s}${a}"
      ).map(_.mkString)
    for {
      name <- xmlNameGen
      internal <- internalGen
      s <- Gen.oneOf(Gen.const(""), xmlWhiteSpaceGen)
    } yield s"<${name}${internal}${s}>"
  }

  /** Generate a valid `Attribute`.
    *
    * Attribute ::= Name Eq AttValue
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Attribute]]
    */
  lazy val xmlAttributeGen: Gen[String] = recursiveStep(
    for {
      name <- xmlNameGen
      eq <- xmlEqGen
      attValue <- xmlAttValueGen
    } yield s"${name}${eq}${attValue}"
  )

  /** Generate a valid `ETag`.
    *
    * ETag ::= '</' Name S? '>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-ETag]]
    */
  lazy val xmlETagGen: Gen[String] = recursiveStep(
    for {
      name <- xmlNameGen
      s <- Gen.oneOf(xmlWhiteSpaceGen, "")
    } yield s"</${name}${s}>"
  )

  /** Generate a valid `content`.
    *
    * content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-content]]
    */
  lazy val xmlContentGen: Gen[String] = recursiveStep{
    val charDataGen: Gen[String] = Gen.oneOf(xmlCharDataGen, Gen.const(""))
    val preambleGen: Gen[String] = Gen.oneOf(
      xmlElementGen,
      xmlReferenceGen,
      xmlCDSectGen,
      xmlPIGen,
      xmlCommentGen
    )
    val restGen: Gen[String] =
      Gen.listOf(
        for {
          p <- preambleGen
          cd <- charDataGen
        } yield s"${p}${cd}"
      ).map(_.mkString)

    for {
      start <- charDataGen
      rest <- restGen
    } yield s"${start}${rest}"
  }

  /** Generate a valid `EmptyElemTag`.
    *
    * EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-EmptyElemTag]]
    */
  lazy val xmlEmptyElemTagGen: Gen[String] = recursiveStep{
    val internalGen: Gen[String] =
      Gen.listOf(
        for {
          s <- xmlWhiteSpaceGen
          a <- xmlAttributeGen
        } yield s"${s}${a}"
      ).map(_.mkString)
    for {
      name <- xmlNameGen
      internal <- internalGen
      s <- Gen.oneOf(Gen.const(""), xmlWhiteSpaceGen)
    } yield s"<${name}${internal}${s}/>"
  }

  /** Generate a valid `elementdecl`
    *
    * elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-elementdecl]]
    */
  lazy val xmlElementDeclGen: Gen[String] = recursiveStep(
    for {
      s0 <- xmlWhiteSpaceGen
      name <- xmlNameGen
      s1 <- xmlWhiteSpaceGen
      contentspec <- xmlContentGen
      s2 <- Gen.oneOf(Gen.const(""), xmlWhiteSpaceGen)
    } yield s"<!ELEMENT${s0}${name}${s1}${contentspec}${s2}>"
  )

  /** Generate a valid `contentspec`.
    *
    * contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-contentspec]]
    */
  lazy val xmlContentSpecGen: Gen[String] = recursiveStep(
    Gen.oneOf(
      Gen.const("EMPTY"),
      Gen.const("ANY"),
      xmlMixedGen,
      xmlChildrenGen
    )
  )

  /** Generate a valid `children`.
    *
    * children ::= (choice | seq) ('?' | '*' | '+')?
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-children]]
    */
  lazy val xmlChildrenGen: Gen[String] = recursiveStep{
    val preamble: Gen[String] =
      Gen.oneOf(
        xmlChoiceGen,
        xmlSeqGen
      )
    val epilogue: Gen[String] = Gen.oneOf(
      Gen.const(""),
      Gen.oneOf("?", "*", "+")
    )

    for {
      p <- preamble
      e <- epilogue
    } yield s"${p}${e}"
  }

  /** Generate a valid `cp`.
    *
    * cp ::= (Name | choice | seq) ('?' | '*' | '+')?
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-cp]]
    */
  lazy val xmlCpGen: Gen[String] = recursiveStep{
    val preamble: Gen[String] = Gen.oneOf( // (Name | choice | seq)
      xmlNameGen,
      xmlChoiceGen,
      xmlSeqGen
    )
    val epilogue: Gen[String] = Gen.oneOf( // ('?' | '*' | '+')?
      Gen.const(""),
      Gen.oneOf("?", "*", "+")
    )

    for {
      p <- preamble
      e <- epilogue
    } yield s"${p}${e}"
  }

  /** Generate a valid `choice`.
    *
    * choice ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-choice]]
    */
  lazy val xmlChoiceGen: Gen[String] = recursiveStep{
    val sGen: Gen[String] = Gen.oneOf(xmlWhiteSpaceGen, Gen.const(""))
    val internalGen: Gen[String] = // ( S? '|' S? cp )+
      Gen.nonEmptyListOf(
        for {
          s0 <- sGen
          s1 <- sGen
          cp <- xmlCpGen
        } yield s"${s0}|${s1}${cp}"
      ).map(_.mkString)
    for {
      s0 <- sGen
      cp <- xmlCpGen
      internal <- internalGen
      s1 <- sGen
    } yield s"(${s0}${cp}${internal}${s1})"
  }

  /** Generate a valid `seq`.
    *
    * seq ::= '(' S? cp ( S? ',' S? cp )* S? ')'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-seq]]
    */
  lazy val xmlSeqGen: Gen[String] = recursiveStep{
    val sGen: Gen[String] = Gen.oneOf(xmlWhiteSpaceGen, Gen.const(""))
    val internalGen: Gen[String] = // ( S? '|' S? cp )*
      Gen.listOf(
        for {
          s0 <- sGen
          s1 <- sGen
          cp <- xmlCpGen
        } yield s"${s0},${s1}${cp}"
      ).map(_.mkString)
    for {
      s0 <- sGen
      cp <- xmlCpGen
      internal <- internalGen
      s1 <- sGen
    } yield s"(${s0}${cp}${internal}${s1})"
  }

  /** Generate a valid `Mixed`.
    *
    * Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' | '(' S? '#PCDATA' S? ')'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Mixed]]
    */
  lazy val xmlMixedGen: Gen[String] = recursiveStep{
    val sGen: Gen[String] = Gen.oneOf(Gen.const(""), xmlWhiteSpaceGen)
    val firstGen: Gen[String] = { // '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
      val internalGen: Gen[String] = // (S? '|' S? Name)*
        Gen.listOf(
          for {
            s0 <- sGen
            s1 <- sGen
            name <- xmlNameGen
          } yield s"${s0}|${s1}${name}"
        ).map(_.mkString)
      for {
        s0 <- sGen
        internal <- internalGen
        s1 <- sGen
      } yield s"(${s0}#PCDATA${internal}${s1})*"
    }
    val secondGen: Gen[String] =
      for {
        s0 <- sGen
        s1 <- sGen
      } yield s"(${s0}#PCDATA${s1})"
    Gen.oneOf(firstGen, secondGen)
  }

  /** Generate a valid `AttlistDecl`.
    *
    * AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-AttlistDecl]]
    */
  lazy val xmlAttlistDeclGen: Gen[String] = recursiveStep(
    for {
      s0 <- xmlWhiteSpaceGen
      name <- xmlNameGen
      attDefs <- Gen.listOf(xmlAttDefGen).map(_.mkString)
      s1 <- Gen.oneOf(Gen.const(""), xmlWhiteSpaceGen)
    } yield s"<!ATTLIST${s0}${name}${attDefs}${s1}>"
  )

  /** Generate a valid `AttDef`.
    *
    * AttDef ::= S Name S AttType S DefaultDecl
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-AttDef]]
    */
  lazy val xmlAttDefGen: Gen[String] = recursiveStep(
    for {
      s0 <- xmlWhiteSpaceGen
      name <- xmlNameGen
      s1 <- xmlWhiteSpaceGen
      attType <- xmlAttTypeGen
      s2 <- xmlWhiteSpaceGen
      defaultDecl <- xmlDefaultDeclGen
    } yield s"${s0}${name}${s1}${attType}${s2}${defaultDecl}"
  )

  /** Generate a valid `AttType`.
    *
    * AttType ::= StringType | TokenizedType | EnumeratedType
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-AttType]]
    */
  lazy val xmlAttTypeGen: Gen[String] =
    recursiveStep(
      Gen.oneOf(xmlStringTypeGen, xmlTokenizedTypeGen, xmlEnumeratedTypeGen)
    )


  /** Generate a valid `StringType`.
    *
    * StringType ::= 'CDATA'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-StringType]]
    */
  lazy val xmlStringTypeGen: Gen[String] =
    Gen.const("CDATA")

  /** Generate a valid `TokenizedType`.
    *
    * TokenizedType ::= 'ID' | 'IDREF'| 'IDREFS' | 'ENTITY' | 'ENTITIES' |
    *                   'NMTOKEN' | 'NMTOKENS'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-TokenizedType]]
    */
  lazy val xmlTokenizedTypeGen: Gen[String] =
    Gen.oneOf(
      "ID", "IDREF", "IDREFS", "ENTITY", "ENTITIES", "NMTOKEN", "NMTOKENS"
    )

  /** Generate a valid `EnumeratedType`.
    *
    * EnumeratedType ::= NotationType | Enumeration
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-EnumeratedType]]
    */
  lazy val xmlEnumeratedTypeGen: Gen[String] =
    recursiveStep(Gen.oneOf(xmlNotationType, xmlEnumerationGen))

  /** Generate a valid `NotationType`.
    *
    * NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-NotationType]]
    */
  lazy val xmlNotationType: Gen[String] = recursiveStep{
    val sGen: Gen[String] = Gen.oneOf(Gen.const(""), xmlWhiteSpaceGen)
    val internalGen: Gen[String] =
      Gen.listOf(
        for {
          s0 <- sGen
          s1 <- sGen
          name <- xmlNameGen
        } yield s"${s0}|${s1}${name}"
      ).map(_.mkString)
    for {
      s0 <- xmlWhiteSpaceGen // _not_ sGen since it is not a S?
      s1 <- sGen
      name <- xmlNameGen
      internal <- internalGen
      s2 <- sGen
    } yield s"NOTATION${s0}(${s1}${name}${internal}${s2})"
  }

  /** Generate a valid `Enumeration`.
    *
    * Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Enumeration]]
    */
  lazy val xmlEnumerationGen: Gen[String] = recursiveStep{
    val sGen: Gen[String] = Gen.oneOf(Gen.const(""), xmlWhiteSpaceGen)
    val internalGen: Gen[String] =
      Gen.listOf(
        for {
          s0 <- sGen
          s1 <- sGen
          nmtoken <- xmlNmtokenGen
        } yield s"${s0}|${s1}${nmtoken}"
      ).map(_.mkString)
    for {
      s0 <- sGen
      nmtoken <- xmlNmtokenGen
      internal <- internalGen
      s1 <- sGen
    } yield s"(${s0}${nmtoken}${internal}${s1})"
  }

  /** Generate a valid `DefaultDecl`.
    *
    * DefaultDecl ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-DefaultDecl]]
    */
  lazy val xmlDefaultDeclGen: Gen[String] = recursiveStep(
    Gen.oneOf(
      Gen.const("#REQUIRED"),
      Gen.const("#IMPLIED"),
      xmlAttValueGen.flatMap(attValue =>
        Gen.oneOf(
          Gen.const(""),
          xmlWhiteSpaceGen.map(s => s"#FIXED${s}")
        ).map(prefix => s"${prefix}${attValue}")
      )
    )
  )

  /** Generate a valid `conditionalSect`.
    *
    * conditionalSect ::= includeSect | ignoreSect
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-conditionalSect]]
    */
  lazy val xmlConditionalSectGen: Gen[String] =
    recursiveStep(Gen.oneOf(xmlIncludeSectGen, xmlIgnoreSectGen))

  /** Generate a valid `includeSect`.
    *
    * includeSect ::= '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-includeSect]]
    */
  lazy val xmlIncludeSectGen: Gen[String] = recursiveStep(
    for {
      s0 <- xmlMaybeWhiteSpaceGen
      s1 <- xmlMaybeWhiteSpaceGen
      e <- xmlExtSubsetDeclGen
    } yield s"<![${s0}INCLUDE${s1}[${e}]]>"
  )

  /** Generate a valid `ignoreSect`.
    *
    * ignoreSect ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-ignoreSect]]
    */
  lazy val xmlIgnoreSectGen: Gen[String] = recursiveStep(
    for {
      s0 <- xmlMaybeWhiteSpaceGen
      s1 <- xmlMaybeWhiteSpaceGen
      isc <- Gen.listOf(xmlIgnoreSectContentsGen)
    } yield s"<![${s0}IGNORE${s1}[${isc.mkString}]]>"
  )

  /** Generate a valid `ignoreSectContents`.
    *
    * ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-ignoreSectContents]]
    */
  lazy val xmlIgnoreSectContentsGen: Gen[String] = recursiveStep{
    val internalGen: Gen[String] =
      for {
        isc <- xmlIgnoreSectContentsGen
        ignore <- xmlIgnoreGen
      } yield s"<![${isc}]]>${ignore}"

      xmlIgnoreGen.flatMap(ignore =>
        Gen.listOf(
          internalGen
        ).map(internal =>
          s"${ignore}${internal.mkString}"
        )
      )
  }

  /** Generate a valid `Ignore`.
    *
    * Ignore ::= Char* - (Char* ('<![' | ']]>') Char*)
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Ignore]]
    */
  lazy val xmlIgnoreGen: Gen[String] = recursiveStep(
    Gen.listOf(xmlCharGen).map(_.mkString).map(value =>
      value.replace("<![", "abc").replace("]]>", "abc")
    )
  )

  /** Generate a valid `EntityDecl`.
    *
    * EntityDecl ::= GEDecl | PEDecl
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-EntityDecl]]
    */
  lazy val xmlEntityDeclGen: Gen[String] =
    recursiveStep(Gen.oneOf(xmlGEDeclGen, xmlPEDeclGen))

  /** Generate a valid `GEDecl`.
    *
    * GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-GEDecl]]
    */
  lazy val xmlGEDeclGen: Gen[String] = recursiveStep(
    for {
      s0 <- xmlWhiteSpaceGen
      n <- xmlNameGen
      s1 <- xmlWhiteSpaceGen
      e <- xmlEntityDefGen
      s2 <- xmlMaybeWhiteSpaceGen
    } yield s"<!ENTITY${s0}${n}${s1}${e}${s2}>"
  )

  /** Generate a valid `PEDecl`.
    *
    * PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PEDecl]]
    */
  lazy val xmlPEDeclGen: Gen[String] = recursiveStep(
    for {
      s0 <- xmlWhiteSpaceGen
      s1 <- xmlWhiteSpaceGen
      n <- xmlNameGen
      s2 <- xmlWhiteSpaceGen
      p <- xmlPEDefGen
      s3 <- xmlWhiteSpaceGen
    } yield s"<!ENTITY${s0}%${s1}${n}${s2}${p}${s3}>"
  )

  /** Generate a valid `EntityDef`.
    *
    * EntityDef ::= EntityValue | (ExternalID NDataDecl?)
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-EntityDef]]
    */
  lazy val xmlEntityDefGen: Gen[String] = recursiveStep{
    val subGen0: Gen[String] =
      for {
        e <- xmlExternalIDGen
        n <- Gen.oneOf(Gen.const(""), xmlNDataDeclGen)
      } yield s"${e}${n}"

    Gen.oneOf(
      xmlEntityValueGen,
      subGen0
    )
  }

  /** Generate a valid `PEDef`.
    *
    * PEDef ::= EntityValue | ExternalID
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PEDef]]
    */
  lazy val xmlPEDefGen: Gen[String] =
    recursiveStep(Gen.oneOf(xmlEntityValueGen, xmlExternalIDGen))

  /** Generate a valid `ExternalID`.
    *
    * ExternalID ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-ExternalID]]
    */
  lazy val xmlExternalIDGen: Gen[String] = recursiveStep{
    val firstGen: Gen[String] =
      for {
        s <- xmlWhiteSpaceGen
        sl <- xmlSystemLiteralGen
      } yield s"SYSTEM${s}${sl}"
    val secondGen: Gen[String] =
      for {
        s0 <- xmlWhiteSpaceGen
        pl <- xmlPubidLiteralGen
        s1 <- xmlWhiteSpaceGen
        sl <- xmlSystemLiteralGen
      } yield s"PUBLIC${s0}${pl}${s1}${sl}"
    Gen.oneOf(firstGen, secondGen)
  }

  /** Generate a valid `NDataDecl`.
    *
    * NDataDecl ::= S 'NDATA' S Name
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-NDataDecl]]
    */
  lazy val xmlNDataDeclGen: Gen[String] = recursiveStep(
    for {
      s0 <- xmlWhiteSpaceGen
      s1 <- xmlWhiteSpaceGen
      n <- xmlNameGen
    } yield s"${s0}NDATA${s1}${n}"
  )

  /** Generate a valid `TextDecl`.
    *
    * TextDecl ::= '<?xml' VersionInfo? EncodingDecl S? '?>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-TextDecl]]
    */
  lazy val xmlTextDeclGen: Gen[String] = recursiveStep(
    for {
      v <- Gen.oneOf(Gen.const(""), xmlVersionInfoGen)
      e <- xmlEncodingDeclGen
      s <- xmlMaybeWhiteSpaceGen
    } yield s"<?xml${v}${e}${s}?>"
  )

  /** Generate a valid `extParsedEnt`.
    *
    * extParsedEnt ::= ( TextDecl? content ) - ( Char* RestrictedChar Char* )
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-extParsedEnt]]
    */
  lazy val xmlExtParsedEntGen: Gen[String] =
    recursiveStep(for {
      textDecl <- Gen.oneOf(Gen.const(""), xmlTextDeclGen)
      content <- xmlContentGen
    } yield s"${textDecl}${content}").map(value =>
      value.map(c =>
        if (xmlRestrictedChars.contains(c)) {
          "a"
        } else {
          c
        }
      ).mkString
    )

  /** Generate a valid `EncodingDecl`.
    *
    * EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-EncodingDecl]]
    */
  lazy val xmlEncodingDeclGen: Gen[String] = recursiveStep{
    val subGen0: Gen[String] =
      Gen.oneOf(
        xmlEncNameGen.map(value => s""""${value}""""),
        xmlEncNameGen.map(value => s"""'${value}'""")
      )

    for {
      s <- xmlWhiteSpaceGen
      e <- xmlEqGen
      sub0 <- subGen0
    } yield s"${s}encoding${e}${sub0}"
  }

  /** Generate a valid `EncName`.
    *
    * EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-EncName]]
    */
  lazy val xmlEncNameGen: Gen[String] = recursiveStep(
    Gen.listOf(
      evenDistribution(
        ('A'.toInt, 'Z'.toInt),
        List(
          ('a'.toInt, 'z'.toInt),
          ('0'.toInt, '9'.toInt),
          ('.'.toInt, '.'.toInt),
          ('_'.toInt, '_'.toInt),
          ('-'.toInt, '-'.toInt)
        )
      ).map(_.toChar)
    ).map(_.mkString)
  )

  /** Generate a valid `NotationDecl`.
    *
    * NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-NotationDecl]]
    */
  lazy val xmlNotationDeclGen: Gen[String] = recursiveStep{
    val subGen0: Gen[String] =
      Gen.oneOf(xmlExternalIDGen, xmlPublicIDGen)

    for {
      s0 <- xmlWhiteSpaceGen
      n <- xmlNameGen
      s1 <- xmlWhiteSpaceGen
      sub0 <- subGen0
      s2 <- xmlMaybeWhiteSpaceGen
    } yield s"<!NOTATION${s0}${n}${s1}${sub0}${s2}>"
  }

  /** Generate a valid `PublicID`.
    *
    * PublicID ::= 'PUBLIC' S PubidLiteral
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PublicID]]
    */
  lazy val xmlPublicIDGen: Gen[String] = recursiveStep(
    for {
      s <- xmlWhiteSpaceGen
      p <- xmlPubidLiteralGen
    } yield s"PUBLIC${s}${p}"
  )

  /** Generate a valid `document`.
    *
    * document ::= ( prolog element Misc* ) - ( Char* RestrictedChar Char* )
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-document]]
    */
  lazy val xmlDocumentGen: Gen[String] = recursiveStep(
    (for {
      p <- xmlPrologGen
      e <- xmlElementGen
      m <- Gen.listOf(xmlMiscGen).map(_.mkString)
    } yield s"${p}${e}${m}").map(value =>
      value.map(c =>
        if (xmlRestrictedChars.contains(c)) {
          "a"
        } else {
          c
        }
      ).mkString
    )
  )
}

object Generators extends Generators
