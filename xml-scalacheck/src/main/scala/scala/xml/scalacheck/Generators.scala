package scala.xml.scalacheck

import org.scalacheck._

trait Generators {

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
        (0x7F to 0x84).toSet
        (0x86 to 0x9F).toSet
    ).map(_.toChar)

  /** Generate a valid XML `Char`.
    *
    * From the specification, "any Unicode character, excluding the surrogate blocks, FFFE, and FFFF.""
    *
    * Char ::= [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Char]]
    */
  lazy val xmlCharGen: Gen[Char] =
    evenDistribution((0x1, 0xD7FF), List((0xE000, 0xFFFD), (0x10000, 0x10FFFF))).map(_.toChar)

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
  lazy val xmlNameGen: Gen[String] =
    for {
      nameStartChar <- xmlNameStartCharGen
      nameChars <- Gen.listOf(xmlNameCharGen).map(_.mkString)
    } yield nameStartChar.toString ++ nameChars

  /** Generate a valid `Names`.
    *
    * Names ::= Name (#x20 Name)*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Names]]
    */
  lazy val xmlNamesGen: Gen[String] =
    for {
      firstName <- xmlNameGen
      rest <- Gen.listOf(xmlNameGen.map(name => s" ${name}")).map(_.mkString)
    } yield firstName ++ rest

  /** Generate a valid `Nmtoken`.
    *
    * Nmtoken ::= (NameChar)+
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Nmtoken]]
    */
  lazy val xmlNmtokenGen: Gen[String] =
    Gen.nonEmptyListOf(xmlNameCharGen).map(_.mkString)

  /** Generate a valid `Nmtokens`.
    *
    * Nmtokens ::= Nmtoken (#x20 Nmtoken)*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Nmtokens]]
    */
  lazy val xmlNmtokensGen: Gen[String] =
    for {
      first <- xmlNmtokenGen
      rest <- Gen.listOf(xmlNmtokenGen.map(nmtoken => s" ${nmtoken}")).map(_.mkString)
    } yield first ++ rest

  /** Generate a valid `Eq`.
    *
    * Eq ::= S? '=' S?
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Eq]]
    */
  lazy val xmlEqGen: Gen[String] =
    for {
      prefixSpace <- Gen.oneOf("", " ")
      suffixSpace <- Gen.oneOf("", " ")
    } yield s"${prefixSpace}=${suffixSpace}"

  /** Generate a valid `EntityRef`.
    *
    * EntityRef ::= '&' Name ';'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-EntityRef]]
    */
  lazy val xmlEntityRefStringGen: Gen[String] =
    xmlNameGen.map(name => s"&${name};")

  /** Generate a valid `CharRef`.
    *
    * CharRef ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-CharRef]]
    */
  lazy val xmlCharRefStringGen: Gen[String] =
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

  /** Generate a valid `Reference`.
    *
    * Reference ::= EntityRef | CharRef
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Reference]]
    */
  lazy val xmlReferenceGen: Gen[String] =
    Gen.oneOf(
      xmlEntityRefStringGen,
      xmlCharRefStringGen
    )

  /** Generate a valid `Reference`.
    *
    * PEReference ::= '%' Name ';'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PEReference]]
    */
  lazy val xmlPEReferenceGen: Gen[String] =
    xmlNameGen.map(value => s"%${value};")

  /** Generate a valid `AttValue`.
    *
    * AttValue ::= '"' ([^<&"] | Reference)* '"'
    *            | "'" ([^<&'] | Reference)* "'"
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-AttValue]]
    */
  lazy val xmlAttValueGen: Gen[String] = {
    def common(delimiter: Char): Gen[String] = {
      val invalid: Set[Char] =
        Set(delimiter, '^', '<', '&')
      Gen.listOf(
        Gen.oneOf(
          xmlCharGen.filterNot(value => invalid.contains(value)).map(_.toString),
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
  lazy val xmlEntityValueGen: Gen[String] = {
    def common(delimiter: Char): Gen[String] = {
      val invalid: Set[Char] =
        Set(delimiter, '^', '%', '&')
      Gen.listOf(
        Gen.oneOf(
          xmlCharGen.filterNot(value => invalid.contains(value)).map(_.toString),
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
  lazy val xmlSystemLiteralGen: Gen[String] = {
    def common(delimiter: Char): Gen[String] =
      Gen.listOf(xmlCharGen.filterNot(_ == delimiter)).map(value =>
        s"${delimiter}${value.mkString}${delimiter}"
      )

    Gen.oneOf(
      common('\''),
      common('"')
    )
  }

  /** Generate a valid `PubidChar`
    *
    * PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PubidChar]]
    */
  lazy val xmlPubidCharGen: Gen[Char] =
    evenDistribution(
      (0x20, 0x20),
      List(
        (0xD, 0xD),
        (0xA, 0xA),
        ('a'.toInt, 'z'.toInt),
        ('A'.toInt, 'Z'.toInt),
        ('0'.toInt, '9'.toInt)
      ) ++
        List('-', '\'', '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*', '#', '@', '$', '_', '%').map(char => (char.toInt, char.toInt))
    ).map(_.toChar)

  /** Generate a valid `PubidLiteral`.
    *
    * PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PubidLiteral]]
    */
  lazy val xmlPubidLiteralGen: Gen[String] = {
    Gen.oneOf(
      Gen.listOf(xmlPubidCharGen).map(value => s""""${value.mkString}""""),
      Gen.listOf(xmlPubidCharGen.filterNot(_ == '\'')).map(value => s"'${value.mkString}'")
    )
  }

  /** Generate a valid `CharData`.
    *
    * CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-CharData]]
    */
  lazy val xmlCharDataGen: Gen[String] = {
    val invalid: Set[Char] =
      Set('^', '<', '&')
    Gen.listOf(xmlCharGen.filterNot(invalid.contains(_))).map(_.mkString).filterNot(_.contains("]]>"))
  }

  /** Generate a valid `Comment`.
    *
    * Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-Comment]]
    */
  lazy val xmlCommentGen: Gen[String] = {
    val minusDash: Gen[String] =
      xmlCharGen.filterNot(_ == '-').map(_.toString)
    Gen.listOf(
      Gen.oneOf(
        minusDash,
        minusDash.map(value => s"-${value}")
      )
    ).map(value => s"<!--${value}-->")
  }

  /** Generate a valid `PITarget`.
    *
    * PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PITarget]]
    */
  lazy val xmlPITargetGen: Gen[String] =
    xmlNameGen.filterNot(
      _.matches("""[xX][mM][lL]""")
    )

  /** Generate a valid `PI`.
    *
    * PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PI]]
    */
  lazy val xmlPIGen: Gen[String] = {
    val restGen: Gen[String] =
      Gen.oneOf(
        for {
          s <- xmlWhiteSpaceGen
          c <- Gen.listOf(xmlCharGen).map(_.mkString).filterNot(_.contains("?>"))
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
  lazy val xmlCDataGen: Gen[String] =
    Gen.listOf(xmlCharGen).map(_.mkString).filterNot(_.contains("]]>"))

  /** Generate a valid `CDSect`.
    *
    * CDSect ::= CDStart CData CDEnd
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-CDSect]]
    */
  lazy val xmlCDSectGen: Gen[String] =
    for {
      cdstart <- xmlCDStartGen
      cdata <- xmlCDataGen
      cdend <- xmlCDEndGen
    } yield s"${cdstart}${cdata}${cdend}"

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
  lazy val xmlMiscGen: Gen[String] =
    Gen.oneOf(
      xmlCommentGen,
      xmlPIGen,
      xmlWhiteSpaceGen
    )

  /** Generate a valid `VersionInfo`.
    *
    * VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-VersionInfo]]
    */
  lazy val xmlVersionInfoGen: Gen[String] = {
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
  lazy val xmlXMLDeclGen: Gen[String] = ???

  /** Generate a valid `prolog`.
    *
    * prolog ::= XMLDecl Misc* (doctypedecl Misc*)?
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-prolog]]
    */
  lazy val xmlPrologGen: Gen[String] = ???

  /** Generate a valid `doctypedecl`.
    *
    * doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-doctypedecl]]
    */
  lazy val xmlDoctypedeclGen: Gen[String] = ???

  /** Generate a valid `DeclSep`.
    *
    * DeclSep ::= PEReference | S
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-DeclSep]]
    */
  lazy val xmlDeclSepGen: Gen[String] =
    Gen.oneOf(
      xmlPEReferenceGen,
      xmlWhiteSpaceGen
    )

  /** Generate a valid `intSubset`.
    *
    * intSubset ::= (markupdecl | DeclSep)*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-intSubset]]
    */
  lazy val xmlIntSubsetGen: Gen[String] =
    Gen.listOf(
      Gen.oneOf(
        xmlMarkupDeclGen,
        xmlDeclSepGen
      )
    ).map(_.mkString)

  /** Generate a valid `markupdecl`.
    *
    * markupdecl ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl |
    *                PI | Comment
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-markupdecl]]
    */
  lazy val xmlMarkupDeclGen: Gen[String] = ???

  /** Generate a valid `extSubset`.
    *
    * extSubset ::= TextDecl? extSubsetDecl
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-extSubset]]
    */
  lazy val xmlExtSubsetGen: Gen[String] = ???

  /** Generate a valid `extSubsetDecl`.
    *
    * extSubsetDecl ::= ( markupdecl | conditionalSect | DeclSep)*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-extSubsetDecl]]
    */
  lazy val xmlExtSubsetDeclGen: Gen[String] = ???

  /** Generate a valid `SDDecl`.
    *
    * SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-SDDecl]]
    */
  lazy val xmlSDDeclGen: Gen[String] = {
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
  lazy val xmlElementGen: Gen[String] =
    Gen.oneOf(
      xmlEmptyElemTagGen,
      for {
        stag <- xmlSTagGen
        content <- xmlContentGen
        etag <- xmlETagGen
      } yield s"${stag}${content}${etag}"
    )

  /** Generate a valid `STag`.
    *
    * STag ::= '<' Name (S Attribute)* S? '>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-STag]]
    */
  lazy val xmlSTagGen: Gen[String] = {
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
  lazy val xmlAttributeGen: Gen[String] =
    for {
      name <- xmlNameGen
      eq <- xmlEqGen
      attValue <- xmlAttValueGen
    } yield s"${name}${eq}${attValue}"

  /** Generate a valid `ETag`.
    *
    * ETag ::= '</' Name S? '>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-ETag]]
    */
  lazy val xmlETagGen: Gen[String] =
    for {
      name <- xmlNameGen
      s <- Gen.oneOf(xmlWhiteSpaceGen, "")
    } yield s"</${name}${s}>"

  /** Generate a valid `content`.
    *
    * content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-content]]
    */
  lazy val xmlContentGen: Gen[String] = {
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
  lazy val xmlEmptyElemTagGen: Gen[String] = {
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
  lazy val xmlElementDeclGen: Gen[String] =
    for {
      s0 <- xmlWhiteSpaceGen
      name <- xmlNameGen
      s1 <- xmlWhiteSpaceGen
      contentspec <- xmlContentGen
      s2 <- Gen.oneOf(Gen.const(""), xmlWhiteSpaceGen)
    } yield s"<!ELEMENT${s0}${name}${s1}${contentspec}${s2}>"

  /** Generate a valid `contentspec`.
    *
    * contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-contentspec]]
    */
  lazy val xmlContentSpecGen: Gen[String] =
    Gen.oneOf(
      Gen.const("EMPTY"),
      Gen.const("ANY"),
      xmlMixedGen,
      xmlChildrenGen
    )

  /** Generate a valid `children`.
    *
    * children ::= (choice | seq) ('?' | '*' | '+')?
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-children]]
    */
  lazy val xmlChildrenGen: Gen[String] = {
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
  lazy val xmlCpGen: Gen[String] = {
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
  lazy val xmlChoiceGen: Gen[String] = {
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
  lazy val xmlSeqGen: Gen[String] = {
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
  lazy val xmlMixedGen: Gen[String] = {
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
  lazy val xmlAttlistDeclGen: Gen[String] =
    for {
      s0 <- xmlWhiteSpaceGen
      name <- xmlNameGen
      attDefs <- Gen.listOf(xmlAttDefGen).map(_.mkString)
      s1 <- Gen.oneOf(Gen.const(""), xmlWhiteSpaceGen)
    } yield s"<!ATTLIST${s0}${name}${attDefs}${s1}>"

  /** Generate a valid `AttDef`.
    *
    * AttDef ::= S Name S AttType S DefaultDecl
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-AttDef]]
    */
  lazy val xmlAttDefGen: Gen[String] =
    for {
      s0 <- xmlWhiteSpaceGen
      name <- xmlNameGen
      s1 <- xmlWhiteSpaceGen
      attType <- xmlAttTypeGen
      s2 <- xmlWhiteSpaceGen
      defaultDecl <- xmlDefaultDeclGen
    } yield s"${s0}${name}${s1}${attType}${s2}${defaultDecl}"

  /** Generate a valid `AttType`.
    *
    * AttType ::= StringType | TokenizedType | EnumeratedType
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-AttType]]
    */
  lazy val xmlAttTypeGen: Gen[String] =
    Gen.oneOf(xmlStringTypeGen, xmlTokenizedTypeGen, xmlEnumeratedTypeGen)


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
    Gen.oneOf(xmlNotationType, xmlEnumerationGen)

  /** Generate a valid `NotationType`.
    *
    * NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-NotationType]]
    */
  lazy val xmlNotationType: Gen[String] = {
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
  lazy val xmlEnumerationGen: Gen[String] = {
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
  lazy val xmlDefaultDeclGen: Gen[String] =
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

  /** Generate a valid `conditionalSect`.
    *
    * conditionalSect ::= includeSect | ignoreSect
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-conditionalSect]]
    */
  lazy val xmlConditionalSectGen: Gen[String] =
    Gen.oneOf(xmlIncludeSectGen, xmlIgnoreSectGen)

  /** Generate a valid `includeSect`.
    *
    * includeSect ::= '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-includeSect]]
    */
  lazy val xmlIncludeSectGen: Gen[String] = ???

  /** Generate a valid `ignoreSect`.
    *
    * ignoreSect ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-ignoreSect]]
    */
  lazy val xmlIgnoreSectGen: Gen[String] =
    for {
      s0 <- xmlMaybeWhiteSpaceGen
      s1 <- xmlMaybeWhiteSpaceGen
      isc <- Gen.listOf(xmlIgnoreSectContentsGen)
    } yield s"<![${s0}IGNORE${s1}[${isc.mkString}]]>"

  /** Generate a valid `ignoreSectContents`.
    *
    * ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-ignoreSectContents]]
    */
  lazy val xmlIgnoreSectContentsGen: Gen[String] = {
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
  lazy val xmlIgnoreGen: Gen[String] =
    Gen.listOf(xmlCharGen).map(_.mkString).filterNot(value =>
      value.contains("<![") || value.contains("]]>")
    )

  /** Generate a valid `EntityDecl`.
    *
    * EntityDecl ::= GEDecl | PEDecl
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-EntityDecl]]
    */
  lazy val xmlEntityDeclGen: Gen[String] = ???

  /** Generate a valid `GEDecl`.
    *
    * GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-GEDecl]]
    */
  lazy val xmlGEDeclGen: Gen[String] = ???

  /** Generate a valid `PEDecl`.
    *
    * PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PEDecl]]
    */
  lazy val xmlPEDeclGen: Gen[String] = ???

  /** Generate a valid `EntityDef`.
    *
    * EntityDef ::= EntityValue | (ExternalID NDataDecl?)
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-EntityDef]]
    */
  lazy val xmlEntityDefGen: Gen[String] = ???

  /** Generate a valid `PEDef`.
    *
    * PEDef ::= EntityValue | ExternalID
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-PEDef]]
    */
  lazy val xmlPEDefGen: Gen[String] = ???

  /** Generate a valid `ExternalID`.
    *
    * ExternalID ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-ExternalID]]
    */
  lazy val xmlExternalIdGen: Gen[String] = {
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
  }

  /** Generate a valid `NDataDecl`.
    *
    * NDataDecl ::= S 'NDATA' S Name
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-NDataDecl]]
    */
  lazy val xmlNDataDeclGen: Gen[String] =
    for {
      s0 <- xmlWhiteSpaceGen
      s1 <- xmlWhiteSpaceGen
      n <- xmlNameGen
    } yield s"${s0}NDATA${s1}${n}"

  /** Generate a valid `TextDecl`.
    *
    * TextDecl ::= '<?xml' VersionInfo? EncodingDecl S? '?>'
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-TextDecl]]
    */
  lazy val xmlTextDeclGen: Gen[String] = ???

  /** Generate a valid `extParsedEnt`.
    *
    * extParsedEnt ::= ( TextDecl? content ) - ( Char* RestrictedChar Char* )
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-extParsedEnt]]
    */
  lazy val xmlExtParsedEntGen: Gen[String] =
    (for {
      textDecl <- Gen.oneOf(Gen.const(""), xmlTextDeclGen)
      content <- xmlContentGen
    } yield s"${textDecl}${content}").filter(value =>
      value.toSet.intersect(xmlRestrictedChars).isEmpty
    )

  /** Generate a valid `EncodingDecl`.
    *
    * EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-EncodingDecl]]
    */
  lazy val xmlEncodingDeclGen: Gen[String] = ???

  /** Generate a valid `EncName`.
    *
    * EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
    *
    * @see [[https://www.w3.org/TR/xml11//#NT-EncName]]
    */
  lazy val xmlEncNameGen: Gen[String] = {
    lazy val epilogue: Gen[String] =
      Gen.oneOf(
        evenDistribution(
          ('A'.toInt, 'Z'.toInt),
          List(
            ('a'.toInt, 'z'.toInt),
            ('0'.toInt, '9'.toInt),
            ('.'.toInt, '.'.toInt),
            ('_'.toInt, '_'.toInt)
          )
        ).map(_.toChar),
        Gen.const('-')
      )
  }
}

object Generators extends Generators
