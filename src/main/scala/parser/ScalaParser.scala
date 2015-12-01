package parser

import util.ParserUtil

import scala.util.parsing.input.Positional

object ScalaParser extends ParserUtil {

  sealed trait Types
  case class IntegerType() extends Types
  case class FloatType() extends Types
  case class DoubleType() extends Types
  case class BooleanType() extends Types
  case class StringType() extends Types
  case class CharacterType() extends Types
  case class SymbolType() extends Types
  case class OtherType() extends Types


  object UnicodeEscapes {

    case class UnicodeEscapeC(str: String) extends Positional

    def UnicodeEscape = positioned(
      ("\\" ~ "u" ~ rep("u") ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
        ^^ { case bs ~ u ~ ul ~ h1 ~ h2 ~ h3 ~ h4 => UnicodeEscapeC("" + bs + u + ul.mkString("") + h1 + h2 + h3 + h4) }
    )

    def hexDigit = """[0-9|a-f|A-F]""".r

  }

  object LexicalSyntax {

    case class LiteralC(str: String, tpe: Types) extends Positional {


    }

    // whiteSpace       ::=  ‘\\u0020’ | ‘\\u0009’ | ‘\\u000D’ | ‘\\u000A’
    // override def whiteSpace = "\\u0020" | "\\u0009" | "\\u000D" | "\\u000A"
    // upper            ::=  ‘A’ | … | ‘Z’ | ‘$’ | ‘_’  // and Unicode category Lu
    def upper = """[A-Z]""".r | "$" | "_"
    // lower            ::=  ‘a’ | … | ‘z’ // and Unicode category Ll
    def lower = """[a-z]""".r
    // letter           ::=  upper | lower // and Unicode categories Lo, Lt, Nl
    def letter = upper | lower
    // digit            ::=  ‘0’ | … | ‘9’
    def digit = """[0-9]""".r
    // paren            ::=  ‘(’ | ‘)’ | ‘[’ | ‘]’ | ‘{’ | ‘}’
    def paren = "(" | ")" | "[" | "]" | "" | ".*"
    // delim            ::=  ‘`’ | ‘'’ | ‘"’ | ‘.’ | ‘;’ | ‘,’
    def delim = "`" | "'" | "\"" | "." | ";" | ","
    // opchar           ::= printableChar not matched by (whiteSpace | upper | lower | letter | digit | paren | delim | opchar | Unicode_Sm | Unicode_So)
    def opchar = printableChar

    // printableChar    ::= // all characters in [\u0020, \u007F] inclusive
    def printableChar = """[\u0020-\u007F]""".r
    // charEscapeSeq    ::= ‘\‘ (‘b‘ | ‘t‘ | ‘n‘ | ‘f‘ | ‘r‘ | ‘"‘ | ‘'‘ | ‘\‘)
    def charEscapeSeq = "\\" ~ ("b" | "t" | "n" | "f" | "r" | "\"" | "\'" | "\\")

    // op               ::=  opchar {opchar}
    def op = opchar ~ opchar.*
    // varid            ::=  lower idrest
    def varid = lower ~ idrest
    // plainid          ::=  upper idrest | varid | op
    def plainid = upper ~ idrest | varid | op
    // id               ::=  plainid | ‘`’ stringLiteral ‘`’
    def id = plainid | "`" ~ stringLiteral ~ "`"
    // idrest           ::=  {letter | digit} [‘_’ op]
    def idrest = (letter | digit).* ~ ("_" ~ op).?

    // integerLiteral   ::=  (decimalNumeral | hexNumeral) [‘L’ | ‘l’]
    def integerLiteral = (hexNumeral | decimalNumeral) ~ ("L" | "l").?
    // decimalNumeral   ::=  ‘0’ | nonZeroDigit {digit}
    def decimalNumeral = "0" | nonZeroDigit ~ digit.*
    // hexNumeral         ::=  ‘0’ (‘x’ | ‘X’) hexDigit {hexDigit}
    def hexNumeral =
      """0[x|X]""".r ~ UnicodeEscapes.hexDigit ~ UnicodeEscapes.hexDigit.*
    // nonZeroDigit     ::=  ‘1’ | … | ‘9’
    def nonZeroDigit =
      """[1-9]""".r

    // floatingPointLiteral  ::=  digit {digit} ‘.’ digit {digit} [exponentPart] [floatType]
    //                           |  ‘.’ digit {digit} [exponentPart] [floatType]  |  digit {digit} exponentPart [floatType]
    //                           |  digit {digit} [exponentPart] floatType
    def floatingPointLiteral = (digit ~ digit.* ~ "." ~ digit ~ digit.* ~ exponentPart.? ~ floatType.?) |
      ("." ~ digit ~ digit.* ~ exponentPart.? ~ floatType.?) | (digit ~ digit.* ~ exponentPart ~ floatType.?) |
      (digit ~ digit.* ~ exponentPart.? ~ floatType)
    // exponentPart     ::=  (‘E’ | ‘e’) [‘+’ | ‘-’] digit {digit}
    def exponentPart = ("E" | "e") ~ ("+" | "-").? ~ digit ~ digit.*
    // floatType        ::=  ‘F’ | ‘f’ | ‘D’ | ‘d’
    def floatType = "F" | "f" | "D" | "d"

    // booleanLiteral   ::=  ‘true’ | ‘false’
    def booleanLiteral = "true" | "false"

    // characterLiteral ::=  ‘'’ (charNoQuoteOrNewline | UnicodeEscape | charEscapeSeq) ‘'’
    def characterLiteral = "\'" ~ ("""[\u0020-\u0026]""".r | """[\u0028-\u007F]""".r | UnicodeEscapes.hexDigit | charEscapeSeq) ~ "\'"
    //def characterLiteral = """'([\u0020-\u0026]|[\u0028-\u007F]|[0-9|a-f|A-F]|\\(b|t|n|f|r|"|'|\\))'""".r
    // "\\" ~ ("b" | "t" | "n" | "f" | "r" | "\"" | "\'" | "\\")
    // stringLiteral    ::=  ‘"’ {stringElement} ‘"’  |  ‘"""’ multiLineChars ‘"""’
    def stringLiteral = "\"\"\"" ~ multiLineChars ~ "\"\"\"" | "\"" ~ stringElement.* ~ "\""

    // stringElement    ::=  charNoDoubleQuoteOrNewline  |  UnicodeEscape    |  charEscapeSeq
    def stringElement = """[\u0020-\u0021]""".r | """[\u0023-\u007F]""".r | UnicodeEscapes.UnicodeEscape | charEscapeSeq
    // multiLineChars   ::=  {[‘"’] [‘"’] charNoDoubleQuote} TODO {‘"’}
    def multiLineChars = ("\"".? ~ "\"".? ~ ("""[\u0020-\u0021]""".r | """[\u0023-\u007F]""".r)).*
    /* ~ rep("\"") */
    // symbolLiteral    ::=  ‘'’ plainid
    def symbolLiteral = "\'" ~ plainid

    // comment          ::=  ‘/*’ “any sequence of characters; nested comments are allowed” ‘*/’
    //                        |  ‘//’ “any sequence of characters up to end of line”
    def comment = "/*" ~ """(.*?)""".r ~ "*/" | "//" ~ """(.*?)""".r ~ nl

    // TODO nl               ::=  “newlinecharacter”
    def nl = "\r".? ~ "\n"
    // semi             ::=  ‘;’ |  nl {nl}
    def semi = ";" | nl ~ nl.*

  }

  object ContextFreeSyntax {

    import LexicalSyntax._

    //    Literal           ::=  [‘-’] integerLiteral  |  [‘-’] floatingPointLiteral  |  booleanLiteral
    //      |  characterLiteral  |  stringLiteral  |  symbolLiteral   |  ‘null’
    def Literal = ("-".? ~ floatingPointLiteral) | ("-".? ~ integerLiteral) | booleanLiteral |
      stringLiteral | characterLiteral | symbolLiteral | "null"

    //    QualId            ::=  id {‘.’ id}
    def QualId = repsep(id, ".")
    //    ids               ::=  id {‘,’ id}
    def ids = repsep(id, ",")
    //    Path              ::=  StableId  |  [id ‘.’] ‘this’
    def Path: Parser[_] = StableId | (id ~ ".").? ~ "this"
    //    StableId          ::=  id   |  Path ‘.’ id   |  [id ‘.’] ‘super’ [ClassQualifier] ‘.’ id
    def StableId = id | Path ~ "." ~ id | (id ~ ".").? ~ "super" ~ ClassQualifier.? ~ "." ~ id
    //    ClassQualifier    ::=  ‘[’ id ‘]’
    def ClassQualifier = "[" ~ id ~ "]"

    //    Type              ::=  FunctionArgTypes ‘=>’ Type   |  InfixType [ExistentialClause]
    def Type: Parser[Any] = FunctionArgTypes ~ "=>" ~ Type | InfixType ~ ExistentialClause.?
    //      FunctionArgTypes  ::= InfixType  | ‘(’ [ ParamType {‘,’ ParamType } ] ‘)’
    def FunctionArgTypes = InfixType | "(" ~ repsep(ParamType, ",").? ~ ")"
    //    ExistentialClause ::=  ‘forSome’ ‘{’ ExistentialDcl {semi ExistentialDcl} ‘}’
    def ExistentialClause = "forSome" ~ "{" ~ ExistentialDcl ~ semi ~ ExistentialDcl.* ~ "}"
    //    ExistentialDcl    ::=  ‘type’ TypeDcl   |  ‘val’ ValDcl
    def ExistentialDcl = "type" ~ TypeDcl | "val" ~ ValDcl
    //    InfixType         ::=  CompoundType {id [nl] CompoundType}
    def InfixType = CompoundType ~ (id ~ nl.? ~ CompoundType).*
    //    CompoundType      ::=  AnnotType {‘with’ AnnotType} [Refinement]   |  Refinement
    def CompoundType = AnnotType ~ "with" ~ AnnotType.* ~ Refinement.? | Refinement
    //      AnnotType         ::=  SimpleType {Annotation}
    def AnnotType: Parser[Any] = SimpleType ~ AnnotType.*
    //    SimpleType        ::=  SimpleType TypeArgs   |  SimpleType ‘#’ id
    //    |  StableId   |  Path ‘.’ ‘type’   |  ‘(’ Types ‘)’
    def SimpleType: Parser[Any] = StableId | Path ~ "." ~ "type" | "(" ~ Types ~ ")" |  SimpleType ~ (TypeArgs | SimpleType ~ "#" ~ id)

    //    TypeArgs          ::=  ‘[’ Types ‘]’
    def TypeArgs = "[" ~ Types ~ "]"
    //    Types             ::=  Type {‘,’ Type}
    def Types = Type ~ repsep(",", Type)
    //    Refinement        ::=  [nl] ‘{’ RefineStat {semi RefineStat} ‘}’
    def Refinement = nl.? ~ "{" ~ RefineStat ~ semi ~ RefineStat.* ~ "}"
    //    RefineStat        ::=  Dcl   |  ‘type’ TypeDef   |
    def RefineStat = Dcl | "type" ~ TypeDef | ""
    //    TypePat           ::=  Type
    def TypePat = Type

    //    Ascription        ::=  ‘:’ InfixType   |  ‘:’ Annotation {Annotation}   |  ‘:’ ‘_’ ‘*’
    def Ascription = ":" ~ InfixType | ":" ~ Annotation.+ | ":" ~ "_" ~ "*"

    //    Expr              ::=  (Bindings | [‘implicit’] id | ‘_’) ‘=>’ Expr   |  Expr1
    def Expr: Parser[Any] = (Bindings | "implicit".? ~ id | "_") ~ "=>" ~ Expr | Expr1
    //      Expr1             ::=  `if' `(' Expr `)' {nl} Expr [[semi] `else' Expr]
    //      |  `while' `(' Expr `)' {nl} Expr   |  `try' (`{' Block `}' | Expr) [`catch' `{' CaseClauses `}'] [`finally' Expr]
    //      |  `do' Expr [semi] `while' `(' Expr ')'  |  `for' (`(' Enumerators `)' | `{' Enumerators `}') {nl} [`yield'] Expr
    //      |  `throw' Expr   |  `return' [Expr]   |  [SimpleExpr `.'] id `=' Expr    |  SimpleExpr1 ArgumentExprs `=' Expr
    //      |  PostfixExpr    |  PostfixExpr Ascription   |  PostfixExpr `match' `{' CaseClauses `}'
    def Expr1 = "if" ~ "(" ~ Expr ~ ")" ~ nl.* ~ Expr ~ (semi.? ~ "else" ~ Expr)
    //      PostfixExpr       ::=  InfixExpr [id [nl]]
    def PostfixExpr = InfixExpr ~ (id ~ nl.?).?
    //      InfixExpr         ::=  PrefixExpr   |  InfixExpr id [nl] InfixExpr
    def InfixExpr: Parser[Any] = PrefixExpr | InfixExpr ~ id ~ nl.? ~ InfixExpr
    //        PrefixExpr        ::=  [‘-’ | ‘+’ | ‘~’ | ‘!’] SimpleExpr
    def PrefixExpr = ("-" | "+" | "~" | "!") ~ SimpleExpr
    //      SimpleExpr        ::=  ‘new’ (ClassTemplate | TemplateBody)
    //      |  BlockExpr   |  SimpleExpr1 [‘_’]
    def SimpleExpr: Parser[Any] = "new" ~ (ClassTemplate | TemplateBody) | BlockExpr | SimpleExpr ~ "_".?
    //      SimpleExpr1       ::=  Literal  |  Path  |  ‘_’   |  ‘(’ [Exprs] ‘)’
    //      |  SimpleExpr ‘.’ id  |  SimpleExpr TypeArgs   |  SimpleExpr1 ArgumentExprs  |  XmlExpr
    def SimpleExpr1: Parser[Any] = Literal | Path | "_" | "(" ~ Expr.? ~ ")" |
      SimpleExpr ~ "." ~ id | SimpleExpr ~ TypeArgs | SimpleExpr1 ~ ArgumentExprs ~ XmlExpressions.XmlExpr
    //        Exprs             ::=  Expr {‘,’ Expr}
    def Exprs = repsep(Expr, ",")
    //      ArgumentExprs     ::=  ‘(’ [Exprs] ‘)’
    //      |  ‘(’ [Exprs ‘,’] PostfixExpr ‘:’ ‘_’ ‘*’ ‘)’  |  [nl] BlockExpr
    def ArgumentExprs = "(" ~ Exprs.? ~ ")" |
      "(" ~ (Exprs ~ ",").? ~ PostfixExpr ~ ":" ~ "_" ~ "*" ~ ")" | nl.? ~ BlockExpr
    //        BlockExpr         ::=  ‘{’ CaseClauses ‘}’  |  ‘{’ Block ‘}’
    def BlockExpr = ("{" ~ CaseClauses ~ "}") | ("{" ~ Block ~ "}")
    //      Block             ::=  BlockStat {semi BlockStat} [ResultExpr]
    def Block = BlockStat ~ (semi ~ BlockStat).* ~ ResultExpr.?
    //      BlockStat         ::=  Import  |  {Annotation} [‘implicit’ | ‘lazy’] Def
    // |  {Annotation} {LocalModifier} TmplDef  |  Expr1   |
    def BlockStat: Parser[Any] = Import | Annotation.? ~ ("implicit" | "lazy") ~ Def | Annotation.* ~ LocalModifier.* ~ TmplDef | ""
    //      ResultExpr        ::=  Expr1  |  (Bindings | ([‘implicit’] id | ‘_’) ‘:’ CompoundType) ‘=>’ Block
    def ResultExpr: Parser[Any] = Expr1 | (Bindings | ("implicit".? ~ id | "_") ~ ":" ~ CompoundType) ~ "=>" ~ Block

    //      Enumerators       ::=  Generator {semi Generator}
    def Enumerators = repsep(Generator, semi)
    //      Generator         ::=  Pattern1 ‘<-’ Expr {[semi] Guard | semi Pattern1 ‘=’ Expr}
    def Generator = Pattern1 ~ "<-" ~ Expr ~ (semi.? ~ Guard | semi ~ Pattern1 ~ "=" ~ Expr)

    //      CaseClauses       ::=  CaseClause { CaseClause }
    def CaseClauses = CaseClause.+
    //      CaseClause        ::=  ‘case’ Pattern [Guard] ‘=>’ Block
    def CaseClause = "case" ~ Pattern ~ Guard.? ~ "=>" ~ Block
    //      Guard             ::=  ‘if’ PostfixExpr
    def Guard: Parser[Any] = "if" ~ PostfixExpr

    //      Pattern           ::=  Pattern1 { ‘|’ Pattern1 }
    def Pattern = repsep(Pattern1, "|")
    //      Pattern1          ::=  varid ‘:’ TypePat  |  ‘_’ ‘:’ TypePat  |  Pattern2
    def Pattern1 = (varid ~ ":" ~ TypePat) | "_" ~ ":" ~ TypePat | Pattern2
    //        Pattern2          ::=  varid [‘@’ Pattern3]  |  Pattern3
    def Pattern2 = varid ~ ("@" ~ Pattern3).? | Pattern3
    //        Pattern3          ::=  SimplePattern  |  SimplePattern { id [nl] SimplePattern }
    def Pattern3 = SimplePattern | repsep(SimplePattern, id ~ nl.?)
    //      SimplePattern     ::=  ‘_’  |  varid  |  Literal  |  StableId
    //        |  StableId ‘(’ Patterns ‘)’  |  StableId ‘(’ [Patterns ‘,’] [varid ‘@’] ‘_’ ‘*’ ‘)’
    //      |  ‘(’ [Patterns] ‘)’  |  XmlPattern
    def SimplePattern = "_" | varid | Literal | StableId |
      StableId ~ "(" ~ Patterns ~ ")" | StableId ~ "(" ~ (Patterns ~ ",").? ~ (varid ~ "@").? ~ "_" ~ "*" ~ ")" |
      "(" ~ Patterns.? ~ ")" | XmlExpressions.XmlPattern
    //        Patterns          ::=  Pattern [‘,’ Patterns]  |  ‘_’ ’*’
    def Patterns: Parser[Any] = repsep(Pattern, ",") | "_" ~ "*"

    //      TypeParamClause   ::=  ‘[’ VariantTypeParam {‘,’ VariantTypeParam} ‘]’
    def TypeParamClause = "[" ~ repsep(VariantTypeParam, ",") ~ "]"
    //      FunTypeParamClause::=  ‘[’ TypeParam {‘,’ TypeParam} ‘]’
    def FunTypeParamClause = "[" ~ repsep(TypeParam, ",") ~ "]"
    //      VariantTypeParam  ::=  {Annotation} [‘+’ | ‘-’] TypeParam
    def VariantTypeParam: Parser[Any] = Annotation.* ~ ("+" | "-") ~ TypeParam
    //      TypeParam         ::=  (id | ‘_’) [TypeParamClause] [‘>:’ Type] [‘<:’ Type] {‘<%’ Type} {‘:’ Type}
    def TypeParam = (id | "_") ~ TypeParamClause.? ~ (">:" ~ Type).? ~ ("<:" ~ Type).? ~ ("<%" ~ Type).* ~ (":" ~ Type).*
    //      ParamClauses      ::=  {ParamClause} [[nl] ‘(’ ‘implicit’ Params ‘)’]
    def ParamClauses = ParamClause.* ~ (nl.? ~ "(" ~ "implicit" ~ Params ~ ")").?
    //      ParamClause       ::=  [nl] ‘(’ [Params] ‘)’
    def ParamClause = nl.? ~ "(" ~ Params.? ~ ")"
    //      Params            ::=  Param {‘,’ Param}
    def Params = repsep(Param, ",")
    //      Param             ::=  {Annotation} id [‘:’ ParamType] [‘=’ Expr]
    def Param = Annotation.* ~ id ~ (":" ~ ParamType).? ~ ("=" ~ Expr).?
    //      ParamType         ::=  Type  |  ‘=>’ Type  |  Type ‘*’
    def ParamType = Type | "=>" ~ Type | Type ~ "*"
    //      ClassParamClauses ::=  {ClassParamClause}  [[nl] ‘(’ ‘implicit’ ClassParams ‘)’]
    def ClassParamClauses = ClassParamClause.* ~ (nl.? ~ "(" ~ "implicit" ~ ClassParams ~ ")").?
    //      ClassParamClause  ::=  [nl] ‘(’ [ClassParams] ‘)’
    def ClassParamClause = nl.? ~ "(" ~ ClassParams.? ~ ")"
    //      ClassParams       ::=  ClassParam {‘,’ ClassParam}
    def ClassParams = repsep(ClassParam, ",")
    //      ClassParam        ::=  {Annotation} {Modifier} [(`val' | `var')]  id ‘:’ ParamType [‘=’ Expr]
    def ClassParam = Annotation.* ~ Modifier.* ~ ("val" | "var").? ~ id ~ ":" ~ ParamType ~ ("=" ~ Expr).?
    //      Bindings          ::=  ‘(’ Binding {‘,’ Binding} ‘)’
    def Bindings = "(" ~ repsep(Binding, ",") ~ ")"
    //      Binding           ::=  (id | ‘_’) [‘:’ Type]
    def Binding = (id | "_") ~ (":" ~ Type).?

    //      Modifier          ::=  LocalModifier  |  AccessModifier  |  ‘override’
    //      LocalModifier     ::=  ‘abstract’  |  ‘final’  |  ‘sealed’  |  ‘implicit’  |  ‘lazy’
    def LocalModifier = "abstract" | "final" | "sealed" | "implicit" | "lazy"
    def Modifier = LocalModifier | AccessModifier | "override"
    //      AccessModifier    ::=  (‘private’ | ‘protected’) [AccessQualifier]
    def AccessModifier: Parser[Any] = ("private" | "protected") ~ AccessModifier.?
    //      AccessQualifier   ::=  ‘[’ (id | ‘this’) ‘]’
    def AccessQualifier = "[" ~ (id | "this") ~ "]"

    //      Annotation        ::=  ‘@’ SimpleType {ArgumentExprs}
    def Annotation = "@" ~ SimpleType ~ ArgumentExprs.*
    //      ConstrAnnotation  ::=  ‘@’ SimpleType ArgumentExprs
    def ConstrAnnotation = "@" ~ SimpleType ~ ArgumentExprs

    //      TemplateBody      ::=  [nl] ‘{’ [SelfType] TemplateStat {semi TemplateStat} ‘}’
    def TemplateBody = nl.? ~ "{" ~ SelfType.? ~ repsep(TemplateStat, semi) ~ "}"
    //      TemplateStat      ::=  Import  |  {Annotation [nl]} {Modifier} Def  |  {Annotation [nl]} {Modifier} Dcl  |  Expr  |
    def TemplateStat: Parser[Any] = Import | (Annotation ~ nl.?).* ~ Modifier.* ~ Def | (Annotation | nl.?).* ~ Modifier.* ~ Dcl | Expr | ""
    //      SelfType          ::=  id [‘:’ Type] ‘=>’   |  ‘this’ ‘:’ Type ‘=>’
    def SelfType = id ~ (":" ~ Type).? ~ "=>" | "this" ~ ":" ~ Type ~ "=>"

    //      Import            ::=  ‘import’ ImportExpr {‘,’ ImportExpr}
    def Import = "import" ~ repsep(ImportExpr, ",")
    //      ImportExpr        ::=  StableId ‘.’ (id | ‘_’ | ImportSelectors)
    def ImportExpr = StableId ~ "." ~ (id | "_" | ImportSelector)
    //      ImportSelectors   ::=  ‘{’ {ImportSelector ‘,’} (ImportSelector | ‘_’) ‘}’
    def ImportSelectors: Parser[Any] = "{" ~ (ImportSelectors ~ ",").* ~ (ImportSelector | "_") ~ "}"
    //      ImportSelector    ::=  id [‘=>’ id | ‘=>’ ‘_’]
    def ImportSelector = id ~ ("=>" ~ id | "=>" ~ "_").?

    //      Dcl               ::=  ‘val’ ValDcl  |  ‘var’ VarDcl  |  ‘def’ FunDcl  |  ‘type’ {nl} TypeDcl
    def Dcl = "val" ~ ValDcl | "var" ~ VarDcl | "def" ~ FunDcl | "type" ~ nl.* ~ TypeDcl

    //      ValDcl            ::=  ids ‘:’ Type
    def ValDcl = ids ~ ":" ~ Type
    //      VarDcl            ::=  ids ‘:’ Type
    def VarDcl = ids ~ ":" ~ Type
    //      FunDcl            ::=  FunSig [‘:’ Type]
    def FunDcl = FunSig ~ (":" ~ Type).?
    //      FunSig            ::=  id [FunTypeParamClause] ParamClauses
    def FunSig = id ~ FunTypeParamClause.? ~ ParamClauses
    //        TypeDcl           ::=  id [TypeParamClause] [‘>:’ Type] [‘<:’ Type]
    def TypeDcl = id ~ TypeParamClause.? ~ (">:" ~ Type).? ~ ("<:" ~ Type)

    //      PatVarDef         ::=  ‘val’ PatDef  |  ‘var’ VarDef
    def PatVarDef = "val" ~ PatDef | "var" ~ VarDef
    //      Def               ::=  PatVarDef   |  ‘def’ FunDef   |  ‘type’ {nl} TypeDef  |  TmplDef
    def Def = PatVarDef | "def" ~ FunDef | "type" ~ nl.* ~ TypeDef | TmplDef
    //        PatDef            ::=  Pattern2 {‘,’ Pattern2} [‘:’ Type] ‘=’ Expr
    def PatDef = repsep(Pattern2, ",") ~ (":" ~ Type).? ~ "=" ~ Expr
    //      VarDef            ::=  PatDef  |  ids ‘:’ Type ‘=’ ‘_’
    def VarDef = PatDef | ids ~ ":" ~ Type ~ "=" ~ "_"
    //      FunDef            ::=  FunSig [‘:’ Type] ‘=’ Expr   |  FunSig [nl] ‘{’ Block ‘}’
    //      |  ‘this’ ParamClause ParamClauses   (‘=’ ConstrExpr | [nl] ConstrBlock)
    def FunDef: Parser[Any] = FunSig ~ opt(":" ~ Type) ~ "=" ~ Expr | FunSig ~ nl.? ~ "{" ~ Block ~ "}" |
      "this" ~ ParamClause ~ ParamClauses ~ ("=" ~ ConstrExpr | nl.? ~ ConstrBlock)
    //      TypeDef           ::=  id [TypeParamClause] ‘=’ Type
    def TypeDef = id ~ TypeParamClause.? ~ "=" ~ Type

    //      TmplDef           ::=  [‘case’] ‘class’ ClassDef  |  [‘case’] ‘object’ ObjectDef  |  ‘trait’ TraitDef
    def TmplDef = "case".? ~ "class".? ~ ClassDef | "case".? ~ "object" ~ ObjectDef | "trait" ~ TraitDef
    //      ClassDef          ::=  id [TypeParamClause] {ConstrAnnotation} [AccessModifier] ClassParamClauses ClassTemplateOpt
    def ClassDef = id ~ TypeParamClause.? ~ ConstrAnnotation.* ~ AccessModifier.? ~ ClassParamClauses ~ ClassTemplateOpt
    //        TraitDef          ::=  id [TypeParamClause] TraitTemplateOpt
    def TraitDef = id ~ TypeParamClause.? ~ TraitTemplateOpt
    //        ObjectDef         ::=  id ClassTemplateOpt
    def ObjectDef = id ~ ClassTemplateOpt
    //        ClassTemplateOpt  ::=  ‘extends’ ClassTemplate | [[‘extends’] TemplateBody]
    def ClassTemplateOpt = "extends" ~ ClassTemplate | "extends".? ~ TemplateBody
    //      TraitTemplateOpt  ::=  ‘extends’ TraitTemplate | [[‘extends’] TemplateBody]
    def TraitTemplateOpt = "extends" ~ TraitTemplate | "extends".? ~ TemplateBody
    //      ClassTemplate     ::=  [EarlyDefs] ClassParents [TemplateBody]
    def ClassTemplate = EarlyDefs.? ~ ClassParents ~ TemplateBody.?
    //      TraitTemplate     ::=  [EarlyDefs] TraitParents [TemplateBody]
    def TraitTemplate = EarlyDefs.? ~ TraitParents ~ TemplateBody.?
    //      ClassParents      ::=  Constr {‘with’ AnnotType}
    def ClassParents = Constr ~ ("with".? ~ AnnotType).*
    //      TraitParents      ::=  AnnotType {‘with’ AnnotType}
    def TraitParents = AnnotType ~ ("with" ~ AnnotType).*
    //      Constr            ::=  AnnotType {ArgumentExprs}
    def Constr: Parser[Any] = AnnotType ~ ArgumentExprs.*
    //      EarlyDefs         ::= ‘{’ [EarlyDef {semi EarlyDef}] ‘}’ ‘with’
    def EarlyDefs = "{" ~ repsep(EarlyDef, "semi") ~ "}" ~ "with"
    //      EarlyDef          ::=  {Annotation [nl]} {Modifier} PatVarDef
    def EarlyDef: Parser[Any] = (Annotation ~ nl.?).* ~ Modifier.* ~ PatVarDef

    //      ConstrExpr        ::=  SelfInvocation  |  ConstrBlock
    def ConstrExpr = SelfInvocation
    //        ConstrBlock       ::=  ‘{’ SelfInvocation {semi BlockStat} ‘}’
    def ConstrBlock = "{" ~ SelfInvocation ~ (semi ~ BlockStat).* ~ "}"
    //      SelfInvocation    ::=  ‘this’ ArgumentExprs {ArgumentExprs}
    def SelfInvocation = "this" ~ ArgumentExprs.+

    //      TopStatSeq        ::=  TopStat {semi TopStat}
    def TopStatSeq: Parser[Any] = repsep(TopStat, semi)
    //      TopStat           ::=  {Annotation [nl]} {Modifier} TmplDef  |  Import  |  Packaging  |  PackageObject  |
    def TopStat = (Annotation ~ nl.?).* ~ Modifier.* ~ TmplDef | Import | Packaging | PackageObject | ""
    //      Packaging         ::=  ‘package’ QualId [nl] ‘{’ TopStatSeq ‘}’
    def Packaging = "package" | QualId ~ nl.? ~ "{" ~ TopStatSeq ~ "}"
    //      PackageObject     ::=  ‘package’ ‘object’ ObjectDef
    def PackageObject = "package" ~ "object" ~ ObjectDef

    //      CompilationUnit   ::=  {‘package’ QualId semi} TopStatSeq
    def CompilationUnit = ("package" ~ QualId ~ semi).* ~ TopStatSeq


  }

  // TODO 色々未実装
  object XmlExpressions {

    // XmlExpr ::= XmlContent {Element}
    def XmlExpr = XmlContent ~ Element.*

    //    Element       ::=    EmptyElemTag   |    STag Content ETag
    def Element = EmptyElemTag | STag ~ Content ~ ETag

    //    EmptyElemTag  ::=    ‘<’ Name {S Attribute} [S] ‘/>’
    def EmptyElemTag = "<" ~ Name ~ (S ~ Attribute).* ~ S.? ~ "/>"

    //    STag          ::=    ‘<’ Name {S Attribute} [S] ‘>’
    def STag = "<" ~ Name ~ (S ~ Attribute).* ~ S.? ~ ">"
    //    ETag          ::=    ‘</’ Name [S] ‘>’
    def ETag = "</" ~ Name ~ S.? ~ ">"
    //    Content       ::=    [CharData] {Content1 [CharData]}
    def Content = CharData.? ~ (Content1 ~ CharData.?).*
    //    Content1      ::=    XmlContent    |    Reference    |    ScalaExpr
    def Content1 = XmlContent | Reference | ScalaExpr
    //      XmlContent    ::=    Element    |    CDSect     |    PI     |    Comment
    def XmlContent: Parser[_] = ??? // Element | CDSect | PI | Comment

    //    Attribute  ::=    Name Eq AttValue
    def Attribute: Parser[_] = ??? // Name ~ Eq ~ AttValue

    //    AttValue      ::=    ‘"’ {CharQ | CharRef} ‘"’   |    ‘'’ {CharA | CharRef} ‘'’   |    ScalaExpr
    def AttValue: Parser[_] = ??? // "\"" ~ (CharQ | CharRef).* ~ "\"" | "\"" ~ (CharA | CharRef).* ~ "\"" | ScalaExpr

    //    ScalaExpr     ::=    Block
    def ScalaExpr: Parser[_] = ??? // Block

    //    CharData      ::=   { CharNoRef }  without {CharNoRef}`{'CharB {CharNoRef}  and without {CharNoRef}`]]>'{CharNoRef}
    def CharData: Parser[_] = ??? // CharaNoRef.*

    //    BaseChar, Char, Comment, CombiningChar, Ideographic, NameChar, S, Reference ::=  “as in W3C XML”
    def BaseChar: Parser[_] = ???
    def Char: Parser[_] = ???
    def Comment: Parser[_] = ???
    def CombiningChar: Parser[_] = ???
    def Ideographic: Parser[_] = ???
    def NameChar: Parser[_] = ???
    def S: Parser[_] = ???
    def Reference: Parser[_] = ???

    //    Char1         ::=  Char  without ‘<’ | ‘&’
    def Char1: Parser[_] = ???
    //    CharQ         ::=  Char1  without ‘"’
    def CharQ: Parser[_] = ???
    //    CharA         ::=  Char1  without ‘'’
    def charA: Parser[_] = ???
    //    CharB         ::=  Char1  without ‘{’
    def CharB: Parser[_] = ???

    //      Name          ::=  XNameStart {NameChar}
    def Name = XNameStart ~ NameChar.*

    //      XNameStart    ::= ‘_’ | BaseChar | Ideographic  (as in W3C XML, but without  ‘:’)
    def XNameStart = "_" | BaseChar | Ideographic

    // XmlPattern  ::= ElementPattern
    def XmlPattern = ElementPattern

    //    ElemPattern   ::=    EmptyElemTagP     |    STagP ContentP ETagP
    def ElementPattern: Parser[_] = EmptyElemTagP | STagP ~ ContentP ~ ETagP

    //    EmptyElemTagP ::=    ‘<’  Name [S] ‘/>’
    def EmptyElemTagP = "<" ~ Name ~ S.? ~ "/>"
    //    STagP         ::=    ‘<’  Name [S] ‘>’
    def STagP = "<" ~ Name ~ S.? ~ ">"
    //    ETagP         ::=    ‘</’ Name [S] ‘>’
    def ETagP = "</" ~ Name ~ S.? ~ ">"
    //    ContentP      ::=    [CharData] {(ElemPattern|ScalaPatterns) [CharData]}
    def ContentP = CharData.? ~ ((ElementPattern | ScalaPatterns) ~ CharData.?).*
    //    ContentP1     ::=    ElemPattern  |  Reference  |  CDSect   |  PI   |   Comment   |   ScalaPatterns
    def ContentP1: Parser[_] = ???
    // ElemPattern | Reference | CDSect | PI | Comment | ScalaPatterns
    //      ScalaPatterns ::=    ‘{’ Patterns ‘}’
    def ScalaPatterns: Parser[_] = ??? // "{" ~ Patterns ~ "}"

  }


}
