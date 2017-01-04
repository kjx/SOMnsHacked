dialect "kernan-prelude"

class s2vWORKS(string) {
  def vec = platform.kernel.Array.new(string.size)
  for (1.asInteger.to(string.size)) do { n ->
      vec.at(n) put(string.at(n))
  }
  method at(i) {vec.at(i.asInteger)}
  method size {string.size}
}

class s2vALSOWORKS(string) {
  def vec = string
  method at(i) {vec.at(i)}
  method size {string.size}
}

method s2v(string) {string}

print "Getting Parsers"



// type Object = { }



// class exports { 


//   method currentIndentation {outer.currentIndetation}
//   method currentIndentation:= (x) {outer.currentIndetation:= x}
//   type InputStream = outer.InputStream
//   class stringInputStream(string : String, position' : Number) {
//         inherit outer.stringInputStream(string, position) }
//   type ParseSuccessType = outer.ParseSuccessType
//   type ParseFailureType = outer.ParseFailureType
//   type ParseResult = (outer.ParseFailureType)
//   class parseSuccess(next', result') {
//      inherit outer.parseSuccess(next', result') }
//   class parseFailure(message') { inherit parseFailure(message') }
//   class abstractParser { inherit abstractParser  }
//   type Parser = (outer.Parser)
//   class tokenParser(tken) { inherit tokenParser(tken) }
//   class whiteSpaceParser { inherit whiteSpaceParser }
//   class characterSetParser(charSet) { inherit characterSetParser(charSet) }
//   class characterSetNotParser(charSet) { inherit characterSetParser(charSet) }
//   class graceIdentifierParser { inherit graceIdentifierParser }
//   class digitStringParser { inherit digitStringParser }
//   class sequentialParser(left, right) { inherit sequentialParser(left, right) }
//   class optionalParser(subParser) { inherit optionalParser(subParser) }
//   class dropParser(subParser) { inherit dropParser(subParser) }
//   class alternativeParser(left, right) { inherit alternativeParser(left, right) }
//   class bothParser(left, right) { inherit bothParser(left, right) }
//   class repetitionParser(subParser) { inherit repetitionParser(subParser) }
//   class proxyParser(proxyBlock) { inherit proxyParser(proxyBlock)  }
//   class wrappingProxyParser(proxyBlock, string) { inherit wrappingProxyParser(proxyBlock, string) }
//   class atEndParser { inherit atEndParser }
//   class notParser(subParser) { inherit notParser(subParser) }
//   class guardParser(subParser, guardBlock) { inherit guardParser(subParser, guardBlock) }
//   class successParser { inherit successParser }
//   class tagParser(tagx : String) { inherit tagParser(tagx) }
//   class phraseParser(tagx: String, subParser) { inherit phraseParser(tagx, subParser) }
//   class indentationAssertionParser(indent : Number) { inherit indentationAssertionParser(indent) }
//   class lineBreakParser(direction) { inherit lineBreakParser(direction)  }
//   method isletter(c) {outer.isletter(c)}
//   method isdigit(c) {outer.isdigit(c)}
//   method dyn(d : Unknown) (outer.dyn(d)}
//   def ws = (outer.ws)
//   method opt(p : Parser)  {outer.opt(p)
//   method rep(p : Parser)  {outer.rep(p)}
//   method rep1(p : Parser) {outer.rep1(p)}
//   method drop(p : Parser) {outer.drop(p)}
//   method trim(p : Parser) {outer.trim(p)}
//   method token(s : String)  {outer.token(s)}
//   method symbol(s : String) {outer.symbol(s)}
//   method rep1sep(p : Parser, q : Parser)  {outer.rep1sep(p,q)}
//   method repsep(p : Parser, q : Parser)  {outer.repsep(p,q)}
//   method repdel(p : Parser, q : Parser)  {outer.repdel(p,q)}
//   method rule(proxyBlock : Block)  {outer.rule(proxyBlock)}
//   method rule(proxyBlock : Block) wrap(s : String) {outer.rule(proxyBlock) wrap(s)}
//   def end = (outer.atEndParser)
//   method not(p : Parser)  {outer.not(p)}
//   method both(p : Parser, q : Parser)  {outer.both(p,q)}
//   method empty  {outer.empty} 
//   method guard(p : Parser, b : Block)  {outer.guard(p, b)} 
//   method tag(s : String) {outer.tag(s)}
//   method phrase(s : String, p : Parser) { outer.phrase(s, p) }
//   method indentAssert(i : Number) { outer.indentAssert(i) }
//   method lineBreak(direction) {outer.lineBreak(direction)}


//SUPER-EVIL GLOBAL VARIABLE Courtesy of MR CHARLES WEIR
var currentIndentation := 0 

////////////////////////////////////////////////////////////
type InputStream = { 
 take( _ ) -> String
 rest ( _ ) -> InputStream
 atEnd -> Boolean
 indentation -> Number
} 

class stringInputStream(string : String, position' : Number) {
 def brand = "stringInputStream"
 def position : Number is readable = position'

 method take(n : Number) -> String {
   var result := ""
   var endPosition := position + n - 1
   if (endPosition > string.size) then {endPosition := string.size}
   for (position.to(endPosition)) do { i : Number ->   
       result := result ++ string.at(i)
   }
   result
 }

 method rest(n : Number)  {

  if ((n + position) <= (string.size + 1))
   then {return stringInputStream(string, position + n)}
   else { 
     Error.raise("FATAL ERROR END OF INPUT at {position} take {n}")
     }
 }

 method atEnd  {position > string.size}

 method indentation {
   var cursor := position - 1
   while {(cursor > 0).andAlso {string.at(cursor) ~= "\n"}}
     do {cursor := cursor - 1}
   // now cursor is the char before the first in the line.
   cursor := cursor + 1
   var result := 0
   while {((cursor + result) <= string.size).andAlso {string.at(cursor + result) == " "}}
     do {result := result + 1}

   if ((cursor + result) > string.size) then {return 0} // lots of spaces then end

   result //return
 }

}

////////////////////////////////////////////////////////////
// parse results

type ParseSuccessType = {
  brand -> String
  next -> InputStream
  result -> String
  succeeded -> Boolean
  resultUnlessFailed( _ ) -> ParseResult 
}

type ParseFailureType = {
  brand -> String
  message -> String
  succeded -> Boolean
  resultUnlessFailed( _ ) -> ParseResult 
}

//type ParseResult = (ParseSuccessType | ParseFailureType)

type ParseResult = ParseFailureType

class parseSuccess(next', result') {
 def brand = "parseSuccess"
 def next is public  = next' 
 def result is public = result'
 method succeeded { true } //return
 method resultUnlessFailed (failBlock : Block) {
   self //return
 }
}

class parseFailure(message') {
 def brand = "parseFailure" 
 def message is public = message'
 method succeeded { false } //return
 method resultUnlessFailed (failBlock : Block) { 
   failBlock.apply(self) //return
 }
}


////////////////////////////////////////////////////////////
// parsers

class abstractParser {
 def brand = "abstractParser"
 method parse(in) { }

 method ~(other) {sequentialParser(self,other)}
 method |(other) {alternativeParser(self,other)}
}

type Parser = { 
  parse(_ : InputStream) -> ParseResult
  ~(_ : Parser) -> Parser
  |(_ : Parser) -> Parser
}

// parse just a token - basically a string, matching exactly
class tokenParser(tken) {
 inherit abstractParser
 def brand = "tokenParser"
 method parse(in) {
   def size = tken.size
   if (in.take(size) == tken) then {
      return parseSuccess(in.rest(size), "{in.take(size)}" )
     } else {
      return parseFailure(
        "expected {tken} got {in.take(size)} at {in.position}")
   }
 }
}

// get at least one whitespace
class whiteSpaceParser {
 inherit abstractParser 
 def brand = "whiteSpaceParser"
 method parse(in) {
   var current := in

   while {(current.take(1) == " ") || (current.take(2) == "\\")} do {
     while {current.take(1) == " "} 
       do {current := current.rest(1)}
     if (current.take(2) == "//")
       then {
         current := current.rest(2)
         while {current.take(1) ~= "\n"} 
           do {current := current.rest(1)}
         current := current.take(1)
       }
   }

   if (current ~= in) then {
      return parseSuccess(current, " ")
     } else {
      return parseFailure(
        "expected w/s got {in.take(5)} at {in.position}")
   }
 }
}


// parse single character from set of acceptable characters (given as a string)
class characterSetParser(charSet) {
 inherit abstractParser
 def brand = "characterSetParser"

 method parse(in) {
   def current = in.take(1) 

   for (charSet) do { c -> 
      if (c == current) then {
        return parseSuccess(in.rest(1), current ) }
     }

   parseFailure( //return
        "expected \"{charSet}\" got {current} at {in.position}")
 }
}

class characterSetNotParser(charSet) {
 inherit abstractParser
 def brand = "characterSetNotParser"

 method parse(in) {
   def current = in.take(1) 

   for (charSet) do { c -> 
      if (c == current) then {
        return parseFailure("expected NOT \"{charSet}\" got {current} at {in.position}") }
     }
   parseSuccess(in.rest(1), current )  //return
 }
}

//does *not* eat whitespace!
class graceIdentifierParser { 
 inherit abstractParser

 def brand = "graceIdentifierParser"

 method parse(in) {
   if (in.take(1) == "_") then {
      return parseSuccess(in.rest(1), "_")                 
   }
   var current := in

   if (! isletter(in.take(1))) then {
      return parseFailure(
        "expected GraceIdentifier got {in.take(5)}... at {in.position}")
   }

   var char := current.take(1)
   var id := ""

   // print "char: <{char}>  current.atEnd <{current.atEnd}>"

   while {(!current.atEnd).andAlso {isletter(char) || isdigit(char) || (char == "'") || (char == "_")}}
     do {
        id := id ++ char
        current := current.rest(1)
        char := current.take(1)
        // print "chlr: <{char}>  current.atEnd <{current.atEnd}>"
   }

   parseSuccess(current, id) //return
 }

}


// dunno why this is here?
class digitStringParser { 
 inherit abstractParser
 def brand = "digitStringParser"
 method parse(in) {

   var current := in

   var char := current.take(1)
   var id := ""

   if (char == "-") then {
     id := "-"
     current := in.rest(1)
     char := current.take(1)     
   }

   if (! (isdigit(char))) then {
      return parseFailure(
        "expected DigitString got {in.take(5)}... at {in.position}")
   }

   while {isdigit(char)}
     do {
        id := id ++ char
        current := current.rest(1)
        char := current.take(1)
   }

   parseSuccess(current, id) //return
 }

}



class sequentialParser(left, right) { 
 inherit abstractParser
 def brand = "sequentialParser"
 method parse(in) {
    def leftResult = left.parse(in)
          .resultUnlessFailed {f -> return f}
    def rightResult = right.parse(leftResult.next)
          .resultUnlessFailed {f -> return f}
    parseSuccess(rightResult.next,  //return
           leftResult.result ++ rightResult.result)
 }
}


class optionalParser(subParser) { 
 inherit abstractParser
 def brand = "optionalParser"
 method parse(in) {
     (subParser.parse(in) //return
          .resultUnlessFailed {f -> 
               return parseSuccess(in, "")})
}

}

//match as if SubParser, discard the result
class dropParser(subParser) {
 inherit abstractParser
 def brand = "dropParser"
 method parse(in) {
    def subRes = subParser.parse(in)
          .resultUnlessFailed {f -> return f}
    parseSuccess(subRes.next, "") //return
 }

}


class alternativeParser(left, right) {
 inherit abstractParser
 def brand = "alternativeParser"
 method parse(in) {
    def leftResult = left.parse(in)
    if (leftResult.succeeded) then {
      return leftResult }
    right.parse(in) //return
 }

}


//succeeds if both left & right succeed; returns LEFT parse
//e.g. both(identifier,not(reservedIdentifiers)) -- except that's wrong!
class bothParser(left, right) {
 inherit abstractParser
 def brand = "bothParser"
 method parse(in) {
    def leftResult = left.parse(in)
    if (!leftResult.succeeded) then {return leftResult}
    def rightResult = right.parse(in)
    if (!rightResult.succeeded) then {return rightResult}
    leftResult //return
 }

}



class repetitionParser(subParser) {
 inherit abstractParser
 def brand = "repetitionParser"
 method parse(in) {
   var current := in

   var res := subParser.parse(in)
   var id := ""

   while {res.succeeded}
     do {
        id := id ++ res.result
        current := res.next
        res := subParser.parse(current)
   }

   parseSuccess(current, id) //return
 }

}



class proxyParser(proxyBlock) { 
 inherit abstractParser
 def brand = "proxyParser"
 var subParser := "no parser installed"
 var needToInitialiseSubParser := true

 method parse(in) {

  if (needToInitialiseSubParser) then {
    subParser := proxyBlock.apply
    needToInitialiseSubParser := false
  }

  def previousIndentation = currentIndentation
  currentIndentation := in.indentation

  var result 

  //  if (currentIndentation < previousIndentation) then {
  //     print ("??Bad Indentation?? at {in.position}, wanted {previousIndentation} got {currentIndentation}")
  //     result := parseFailure("Bad Indentation, wanted {previousIndentation} got {currentIndentation}")
  //  } else {
  result := subParser.parse(in)
  //  }

  currentIndentation := previousIndentation

  result //return
 }
}



class wrappingProxyParser(proxyBlock, string) {
 inherit abstractParser
 def brand = "wrappingProxyParser"
 var subParser := "no parser installed"
 var needToInitialiseSubParser := true

 method parse(in) {

  if (needToInitialiseSubParser) then {
    subParser := proxyBlock.apply
    needToInitialiseSubParser := false
  }

  def result = subParser.parse(in)
  if (!result.succeeded) then {return result}

  parseSuccess(result.next, "[{string}{result.result}]") //return
 }

}



// get at least one whitespace
class atEndParser { 
 inherit abstractParser
 def brand = "atEndParser"
 method parse(in) {
   if (in.atEnd) then {
      return parseSuccess(in, "")
     } else {
      return parseFailure(
        "expected end got {in.take(5)} at {in.position}")
   }
 }

}

// succeeds when subparser fails; never consumes input if succeeds
class notParser(subParser) {
 inherit abstractParser
 def brand = "notParser"
 method parse(in) {
    def result = subParser.parse(in)

    if (result.succeeded)
      then {return parseFailure("Not Parser - subParser succeeded so I failed")}
      else {return parseSuccess(in,"")}
 }

}


class guardParser(subParser, guardBlock) {
 inherit abstractParser
 def brand = "guardParser"
 method parse(in) {
    def result = subParser.parse(in)

    if (!result.succeeded) then {return result}
    if (guardBlock.apply(result.result)) then {return result}
    parseFailure("Guard failure at {in.position}") //return
 }

}


class successParser {
 inherit abstractParser
 def brand = "successParser"
 method parse(in) {parseSuccess(in,"!!success!!")} //return

}


// puts tag into output
class tagParser(tagx : String) {
 inherit abstractParser
 def brand = "tagParser"
 method parse(in) {parseSuccess(in, tagx)} //return

}

// puts tagx around start and end of parse
class phraseParser(tagx: String, subParser) {
 inherit abstractParser
 def brand = "phraseParser"
 method parse(in) {
    def result = subParser.parse(in)

    if (!result.succeeded) then {return result}

    parseSuccess(result.next, //return
              "<" ++ tagx ++ " " ++ result.result ++ " " ++ tagx ++ ">" )
 }

}


class indentationAssertionParser(indent : Number) {
 inherit abstractParser
 def brand = "indentationAssertionParser"
 method parse(in) {
   if (in.indentation == indent) 
    then {return parseSuccess(in,"")}
    else { print  "***Asserted indent=={indent}, actual indentation=={in.indentation}"
           return parseFailure "Asserted indent=={indent}, actual indentation=={in.indentation}"}
 }
}


class lineBreakParser(direction) { 
 inherit abstractParser
 def brand = "lineBreakParser"
 method parse(in) {

  if (in.take(1) ~= "\n") 
    then {return parseFailure "looking for a LineBreak-{direction}, got \"{in.take(1)}\" at {in.position}"}

  def rest = in.rest(1) 

  def actualDirection = 
    if (rest.atEnd) then { "left" }
      elseif {rest.indentation <  currentIndentation} then { "left" }
      elseif {rest.indentation == currentIndentation} then { "same" }
      elseif {rest.indentation >  currentIndentation} then { "right" }
      else {Error.raise "Tricotomy failure"}


  if (direction.match(actualDirection))  then {
       return parseSuccess(in.rest(1), "<<{direction}>>\n" ) 
     }

  parseFailure "looking for a LineBreak-{direction}, got {actualDirection} at {in.position}" 


  // Error.raise "Shouldn't happen"
 }
}





////////////////////////////////////////////////////////////
// combinator functions - many of these should be methods
// on parser but I got sick of copying everything!

def ws = rep1((whiteSpaceParser) | lineBreak("right"))
method opt(p : Parser)  {optionalParser(p)}
method rep(p : Parser)  {repetitionParser(p)}
method rep1(p : Parser) {p ~ repetitionParser(p)}
method drop(p : Parser) {dropParser(p)}
method trim(p : Parser) {drop(opt(ws)) ~ p ~ drop(opt(ws))}
method token(s : String)  {tokenParser(s)}
//method symbol(s : String) {trim(token(s))}
method symbol(s : String) {token(s) ~ drop(opt(ws))} // changed to token with following space?
method rep1sep(p : Parser, q : Parser)  {p ~ rep(q ~ p)}
method repsep(p : Parser, q : Parser)  {opt( rep1sep(p,q))}
method repdel(p : Parser, q : Parser)  {repsep(p,q) ~ opt(q)}
method rule(proxyBlock : Block)  {proxyParser(proxyBlock)}
//method rule(proxyBlock : Block) wrap(s : String)  {wrappingProxyParser(proxyBlock,s)}
method rule(proxyBlock : Block) wrap(s : String)  {proxyParser(proxyBlock,s)}

def end = atEndParser
method not(p : Parser)  {notParser(p)}
method both(p : Parser, q : Parser)  {bothParser(p,q)}
method empty  {successParser} 
method guard(p : Parser, b : Block)  {guardParser(p, b)} 
method tag(s : String) {tagParser(s)}
method phrase(s : String, p : Parser) { phraseParser(s, p) }
method indentAssert(i : Number) {indentationAssertionParser(i) }

method lineBreak(direction) {lineBreakParser(direction)}

method parse (s : String) with (p : Parser)  {
 p.parse(stringInputStream(s,1)).succeeded
}


////////////////////////////////////////////////////////////
// "support library methods" 



method isletter(c) -> Boolean {
  // print "callxd isletter({c})"
  if (c.size == 0) then {return false} //is bad. is a hack. works.
  if (c.size ~= 1) then {print "isletter string {c} too long"}
  //  assert {c.size == 1} complaint "isletter string too long" 
  (((c >= "A") && (c <= "Z"))  //return
         || ((c >= "a") && (c <= "z")))
}

method isdigit(c) -> Boolean {
  // print "callxd isdigit({c})"
  //  assert {c.size == 1} complaint "isdigit string too long" 
  if (c.size == 0) then {return false} //is bad. is a hack. works. 
  ((c >= "0") && (c <= "9"))  //return
}

print "Got Parsers"



//////////////////////////////////////////////////
// Grace Grammar


//BEGINGRAMMAR
// top level
def program = rule {codeSequence ~ rep(ws) ~ end}
def codeSequence = rule { repdel((declaration | statement | empty), semicolon) }
def innerCodeSequence = rule { repdel((innerDeclaration | statement | empty), semicolon) }

def moduleHeader = rule { rep(hashLine) ~ rep(importStatement | reuseClause) } 
def hashLine = rule { (symbol "#") ~ rep(anyChar | space) ~ (newLine | end) }
def importStatement = rule { importId ~ stringLiteral ~ asId ~ identifier ~ semicolon } 
 
// def comment = 

// declarations

def declaration = rule { varDeclaration | defDeclaration | classOrTraitDeclaration | typeDeclaration | methodDeclaration }

def innerDeclaration = rule { varDeclaration | defDeclaration | classOrTraitDeclaration | typeDeclaration }

def varDeclaration = rule { varId ~ identifier ~  opt(colon ~ typeExpression) ~ opt(assign ~ expression) }

def defDeclaration = rule { defId ~ identifier ~  opt(colon ~ typeExpression) ~ equals ~ expression }

def methodDeclaration = rule { methodId ~ methodHeader ~ methodReturnType ~ whereClause ~ lBrace ~ innerCodeSequence ~ rBrace }

def classOrTraitDeclaration = rule { (classId | traitId) ~ classHeader ~ methodReturnType ~ whereClause ~ lBrace ~ rep(reuseClause) ~ codeSequence ~ rBrace }

//def oldClassDeclaration = rule { classId ~ identifier ~ lBrace ~ 
//                             opt(genericFormals ~ blockFormals ~ arrow) ~ codeSequence ~ rBrace }


//warning: order here is significant!
def methodHeader = rule {  assignmentMethodHeader | methodWithArgsHeader | unaryMethodHeader | operatorMethodHeader | prefixMethodHeader  } 

def classHeader = methodHeader    // rule { methodWithArgsHeader | unaryMethodHeader }
def reuseClause = rule { (inheritId | useId) ~ expression ~ semicolon ~ rep(reuseModifiers) }  
def reuseModifiers = rule { excludeClause | aliasClause } 
def excludeClause = rule { excludeId ~ methodHeader ~ semicolon }
def aliasClause = rule { aliasId ~ methodHeader ~ equals ~ methodHeader ~ semicolon }

def unaryMethodHeader = rule { identifier ~ genericFormals } 
def methodWithArgsHeader = rule { firstArgumentHeader ~ repsep(argumentHeader,opt(ws)) }
def firstArgumentHeader = rule { identifier ~ genericFormals ~ methodFormals }
def argumentHeader = rule { identifier ~ methodFormals }
def operatorMethodHeader = rule { otherOp ~ genericFormals ~ oneMethodFormal } 
def prefixMethodHeader = rule { opt(ws) ~ token("prefix") ~ otherOp ~ genericFormals }
def assignmentMethodHeader = rule { identifier ~ assign ~ genericFormals ~ oneMethodFormal }

def methodReturnType = rule { opt(arrow ~ nonEmptyTypeExpression )  } 

def methodFormals = rule { lParen ~ rep1sep( identifier ~ opt(colon ~ opt(ws) ~ typeExpression), comma) ~ rParen}
def oneMethodFormal = rule { lParen ~ identifier ~ opt(colon ~ typeExpression) ~ rParen}
def blockFormals = rule { repsep( identifier ~ opt(colon ~ typeExpression), comma) }

def matchBinding = rule { (stringLiteral | numberLiteral | (lParen ~ identifier ~ rParen)) ~ opt(colon ~ nonEmptyTypeExpression) }


def matchBindingOLD = rule{ (literal | parenExpression | identifier)
                             ~ opt(colon ~ nonEmptyTypeExpression ~ opt(matchingBlockTail)) }

def matchingBlockTail = rule { lParen ~ rep1sep(matchBinding, comma)  ~ rParen }

def typeDeclaration = rule { typeId ~ identifier ~ genericFormals ~ equals ~ nonEmptyTypeExpression ~ (semicolon | whereClause)}

//these are the things that work - 24 July with EELCO
def typeExpression = rule { (opt(ws) ~ typeOpExpression ~ opt(ws)) | opt(ws) }
def nonEmptyTypeExpression = rule { opt(ws) ~ typeOpExpression ~ opt(ws) }

//these definitely don't - 24 July with EELCO
// def typeExpression = rule { (opt(ws) ~ expression ~ opt(ws)) | opt(ws) }
//def nonEmptyTypeExpression = rule { opt(ws) ~ expression ~ opt(ws) }

def typeOp = rule { opsymbol("|") | opsymbol("&") | opsymbol("+") } 

// def typeOpExpression = rule { rep1sep(basicTypeExpression, typeOp) }

// this complex rule ensures two different typeOps have no precedence
def typeOpExpression = rule {  
  var otherOperator 
  basicTypeExpression ~ opt(ws) ~
    opt( guard(typeOp, { s -> otherOperator:= s;
                              true }) ~ rep1sep(basicTypeExpression ~ opt(ws),
             guard(typeOp, { s -> s == otherOperator })
        )
    )
  } 

def basicTypeExpression = rule { nakedTypeLiteral | literal | pathTypeExpression | parenTypeExpression }  
   // if we keep this, note that in a typeExpression context { a; } is  interpreted as type { a; }
   // otherwise as the block { a; }

def pathTypeExpression = rule { opt(listOfOuters ~ dot) ~ rep1sep((identifier ~ genericActuals),dot) }

def parenTypeExpression = rule { lParen ~ typeExpression ~ rParen } 



// statements

def statement = rule { returnStatement | (expression ~ opt(assignmentTail))  } 
    // do we need constraints here on which expressions can have an assignmentTail
    // could try to rewrite as options including (expression ~ arrayAccess ~ assignmentTail)
    // expression ~ dot ~ identifier ~ assignmentTail 

def returnStatement = rule { returnId ~ opt(expression) }  //doesn't need parens
def assignmentTail = rule { assign ~ expression }

// expressions

def expression = rule { opExpression } 

//def opExpression = rule { rep1sep(addExpression, otherOp)}

// this complex rule ensures two different otherOps have no precedence
def opExpression = rule { 
  var otherOperator 
  addExpression ~ opt(ws) ~
    opt( guard(otherOp, { s -> otherOperator:= s;
                               true }) ~ rep1sep(addExpression ~ opt(ws),
           guard(otherOp, { s -> s == otherOperator })
        )
    )
  } 

def addExpression = rule { rep1sep(multExpression, addOp) }
def multExpression = rule { rep1sep(prefixExpression, multOp) }
def prefixExpression = rule { (opt(otherOp) ~ selectorExpression) | (otherOp ~ listOfOuters) } 
      // we can have !outer or !outer.outer

def selectorExpression = rule { primaryExpression ~ rep(selector) }

def selector = rule { (dot ~ unaryRequest) | (dot ~ requestWithArgs) }

def operatorChar = characterSetParser("!?@#$%^&|~=+-*/><:.") // had to be moved up

//special symbol for operators: cannot be followed by another operatorChar
method opsymbol(s : String) {trim(token(s) ~ not(operatorChar))}

def multOp = opsymbol "*" | opsymbol "/" 
def addOp = opsymbol "+" | opsymbol "-" 
def otherOp = rule { guard(trim(rep1(operatorChar)), { s -> ! parse(s) with( reservedOp ~ end ) })} 
    // encompasses multOp and addOp
def operator = rule { otherOp | reservedOp }  

def unaryRequest = rule { trim(identifier) ~ genericActuals ~ not(delimitedArgument) } 
def requestWithArgs = rule { firstRequestArgumentClause ~ repsep(requestArgumentClause,opt(ws)) }
def firstRequestArgumentClause = rule { identifier ~ genericActuals ~ opt(ws) ~ delimitedArgument }
def requestArgumentClause = rule { identifier ~ opt(ws) ~ delimitedArgument }
def delimitedArgument = rule { argumentsInParens | blockLiteral | stringLiteral }
def argumentsInParens = rule { lParen ~ rep1sep(drop(opt(ws)) ~ expression, comma) ~ rParen  }  

def implicitSelfRequest = rule { requestWithArgs |  rep1sep(unaryRequest,dot) }

def primaryExpression = rule { literal | listOfOuters | implicitSelfRequest | parenExpression }  

def parenExpression = rule { lParen ~ rep1sep(drop(opt(ws)) ~ expression, semicolon) ~ rParen } 

// def nonNakedSuper = rule { superId ~ not(not( operator | lBrack )) }
def listOfOuters = rule { rep1sep(outerId, dot) }

// "generics" 
def genericActuals = rule { opt(lGeneric ~ opt(ws) ~ rep1sep(opt(ws) ~ typeExpression ~ opt(ws), opt(ws) ~ comma ~ opt(ws)) ~ opt(ws) ~ rGeneric) }

def genericFormals = rule {  opt(lGeneric ~ rep1sep(identifier, comma) ~ rGeneric) }

def whereClause = rule { repdel(whereId ~ typePredicate, semicolon) }
def typePredicate = rule { expression }

//wherever genericFormals appear, there should be a whereClause nearby.


// "literals"

def literal = rule { stringLiteral | selfLiteral | blockLiteral | numberLiteral | objectLiteral | lineupLiteral | typeLiteral } 

def stringLiteral = rule { opt(ws) ~ doubleQuote ~ rep( stringChar ) ~ doubleQuote ~ opt(ws) } 
def stringChar = rule { (drop(backslash) ~ escapeChar) | anyChar | space }
def blockLiteral = rule { lBrace ~ opt(ws) ~ opt(genericFormals ~ opt(matchBinding) ~ blockFormals ~ opt(ws) ~ arrow) ~ innerCodeSequence ~ rBrace }
def selfLiteral = symbol "self" 
def numberLiteral = trim(digitStringParser)
def objectLiteral = rule { objectId ~ lBrace ~ rep(reuseClause) ~ codeSequence ~ rBrace }

//these are *not* in the spec - EELCO 
def lineupLiteral = rule { lBrack ~ repsep( expression, comma ) ~ rBrack }

def typeLiteral = rule { typeId ~ opt(ws) ~ nakedTypeLiteral }

//kernan
def nakedTypeLiteral = rule { lBrace ~ opt(ws) ~ repdel(methodHeader ~ methodReturnType, (semicolon | whereClause)) ~ opt(ws) ~ rBrace }

// terminals
def backslash = token "\\"    // doesn't belong here, doesn't work if left below!
def doubleQuote = token "\""
def space = token " " 

def semicolon = rule { (symbol(";") ~ opt(newLine)) | (opt(ws) ~ lineBreak("left" | "same") ~ opt(ws)) }

// broken
// def semicolon = rule { (symbol(";") ~ opt(newLine)) | (opt(ws) ~ lineBreak("left" | "same") ~ opt(ws)) | end }

def colon = rule {both(symbol ":", not(assign))}
def newLine = symbol "\n" 
def lParen = symbol "("
def rParen = symbol ")" 
def lBrace = symbol "\{"
def rBrace = symbol "\}"
def lBrack = rule {both(symbol "[", not(lGeneric))}
def rBrack = rule {both(symbol "]", not(rGeneric))}
def arrow = symbol "->"
def dot = symbol "."
def assign = symbol ":="
def equals = symbol "="

def lGeneric = symbol "[["
def rGeneric = symbol "]]"

def comma = rule { symbol(",") }
def escapeChar = characterSetParser("\\\"'\{\}bnrtlfe ")

def azChars = "abcdefghijklmnopqrstuvwxyz"
def AZChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
def otherChars = "1234567890~!@#$%^&*()_-+=[]|\\:;<,>.?/"

def anyChar = characterSetParser(azChars ++ AZChars ++ otherChars)

def identifierString = (graceIdentifierParser ~ drop(opt(ws)))

// def identifier = rule { bothAll(trim(identifierString),not(reservedIdentifier))  }   
                           // bothAll ensures parses take the same length
// def identifier = rule{ both(identifierString,not(reservedIdentifier))  }   
                          // both doesn't ensure parses take the same length
def identifier = rule { guard(identifierString, { s -> ! parse(s) with( reservedIdentifier ~ end ) })}
                        // probably works but runs out of stack

// anything in this list needs to be in reservedIdentifier below (or it won't do what you want)
def aliasId = symbol "alias"
def asId = symbol "as"
def classId = symbol "class" 
def defId = symbol "def" 
def dialectId = symbol "dialect"
def excludeId = symbol "exclude"
def importId = symbol "import"
def inheritId = symbol "inherit"
def interfaceId = symbol "interface"
def isId = symbol "is"
def methodId = symbol "method" 
def objectId = symbol "object" 
def outerId = symbol "outer" 
def prefixId = symbol "prefix" 
def requiredId = symbol "required"
def returnId = symbol "return"
def traitId = symbol "trait" 
def typeId = symbol "type" 
def useId = symbol "use"
def varId = symbol "var" 
def whereId = symbol "where" 

//kernan
def reservedIdentifier = rule {selfLiteral | aliasId |  asId |  classId |  defId |  dialectId |  excludeId |  importId |  inheritId | isId |  methodId | objectId | outerId | prefixId |  requiredId |  returnId | traitId |  typeId |  useId |  varId |  whereId}

//oops deleted interfaceId

def reservedOp = rule {assign | equals | dot | arrow | colon | semicolon}  // this is not quite right


print "done grammar.grace"




























////////////////////////////////////////////////////////////
// "tests" 


method assert  (assertion : Block) complaint (name : String) {
  if (!assertion.apply) 
    then {print "ASSERTION FAILURE"}
}


print("start")

var passedTests := 0
var failedTests := 0

def printPassedTests = false

method test (block : Block, result : Object, comment : String) {
  def rv = block.apply
  if  (rv == result) 
    then {if  (printPassedTests) then {print  ("------: " ++ comment)} else {print "."}}
    else {if (!printPassedTests) then {print ""}
          print  ("FAILED: " ++ comment)}
}

method test(block : Block) expecting(result : Object) comment(comment : String) {
   test(block,result,comment)
}


// method test (block : Block, result : Object, comment : String) {
//  def rv = block.apply
//  if  (rv == result) 
//    then {print  ("------: " ++ comment)}
//    else {print  ("FAILED: " ++ comment)} 
// }

method test(parser : Parser) on(s : String) correctly(comment : String) {
  def res = parser.parse(stringInputStream(s2v(s),1))
  if (res.succeeded) 
    then {if (printPassedTests) then {print  ("------: " ++ comment ++ " " ++  res.result)}  else {print "."}}
    else {
       if (!printPassedTests) 
          then {print ""}
       print  ("FAILED: " ++ comment ++ " " ++  s)
     }
}

method test(parser : Parser) on(s : String) wrongly(comment : String) {
  def rv = parser.parse(stringInputStream(s2v(s),1)).succeeded
  if  (!rv) 
    then {if (printPassedTests) then {print  ("------: " ++ comment ++ " " ++  s)}  else {print "."}}
    else {
       if (!printPassedTests) then {print ""}
       print  ("FAILED: " ++ comment ++ " " ++  s)
    } 
}


method testProgramOn(s : String) correctly(comment : String) {
  test(program) on(s) correctly(comment)
}

method testProgramOn(s : String) wrongly(comment : String) {
  test(program) on(s) wrongly(comment)
}



print "HERE WE GO"



























def strm   = stringInputStream(s2v "Hello World",1)


def strm2  = stringInputStream(s2v "   Hello World",1)
def strmus = stringInputStream(s2v "_",1)
def strmab  = stringInputStream(s2v "abc4de'a123",1)
def strmas  = stringInputStream(s2v "a bcb", 1)
def strmnn  = stringInputStream(s2v "1234  ",1)
def strmnx  = stringInputStream(s2v "1234",1)
def strmxx  = stringInputStream(s2v "xxxxx",1)
def strmxc  = stringInputStream(s2v "xcxcxf",1)
def strmcx  = stringInputStream(s2v "xcxcf",1)
def strmx  = stringInputStream(s2v "xf",1)
def strmxcx  = stringInputStream(s2v "xcxf",1)

//index          123 45678 90123
//indent         111 22222 0000" 
def indentStx = " 11\n  22\nnone"




currentIndentation := 0


test { stringInputStream(s2v(indentStx), 1 ).indentation } expecting (1) comment "ix indent at start of line"

print "DONE STRM"


test { stringInputStream(s2v(indentStx), 3 ).indentation } expecting (1) comment "Indentation 2"
test { stringInputStream(s2v(indentStx), 4 ).indentation } expecting (1) comment "ix EOL belongs to previous line"
test { stringInputStream(s2v(indentStx), 5 ).indentation } expecting (2) comment "Indentation 5"
test { stringInputStream(s2v(indentStx), 10 ).indentation } expecting (0) comment "Indentation 10"
test { stringInputStream(s2v(indentStx), 7 ).indentation } expecting (2) comment "Indentation 1"
test { stringInputStream(s2v(indentStx), s2v(indentStx).size + 1 ).indentation } expecting (0) comment "Indentation Line end"
test { stringInputStream(s2v(indentStx), s2v(indentStx).size + 1 ).indentation } expecting (0) comment "Indentation Line end"
test { stringInputStream("print(1)", 3 ).indentation } expecting (0) comment "print(0)"



currentIndentation := 0
test { lineBreak("left").parse(stringInputStream(s2v(indentStx), 1)).succeeded } expecting (false) comment "cnl1"
test { lineBreak("left").parse(stringInputStream(s2v(indentStx), 2)).succeeded } expecting (false) comment "cnl2"
test { lineBreak("left").parse(stringInputStream(s2v(indentStx), 4)).succeeded } expecting (false) comment "cnl4"
test { lineBreak("left").parse(stringInputStream(s2v(indentStx), 5)).succeeded } expecting (false) comment "cnl5"
test { lineBreak("left").parse(stringInputStream(s2v(indentStx), 7)).succeeded } expecting (false) comment "cnl7"
test { lineBreak("left").parse(stringInputStream(s2v(indentStx), 9)).succeeded } expecting (false) comment "cnl9"
test { lineBreak("left").parse(stringInputStream(s2v(indentStx), 10)).succeeded } expecting (false) comment "cnl10"

currentIndentation := 1
test { lineBreak("left").parse(stringInputStream(s2v(indentStx), 4)).succeeded } expecting (false) comment "cnl4-1"
test { lineBreak("left").parse(stringInputStream(s2v(indentStx), 9)).succeeded } expecting (true) comment "cnl9-1"

currentIndentation := 2
test { lineBreak("left").parse(stringInputStream(s2v(indentStx), 4)).succeeded } expecting (false) comment "cnl4-2"
test { lineBreak("left").parse(stringInputStream(s2v(indentStx), 9)).succeeded } expecting (true) comment "cnl9-2"

currentIndentation := 0
test { lineBreak("right").parse(stringInputStream(s2v(indentStx), 4)).succeeded } expecting (true) comment "cnl4-3"
test { lineBreak("right").parse(stringInputStream(s2v(indentStx), 9)).succeeded } expecting (false) comment "cnl9-3"

currentIndentation := 1
test { lineBreak("right").parse(stringInputStream(s2v(indentStx), 4)).succeeded } expecting (true) comment "cnl4-4"
test { lineBreak("right").parse(stringInputStream(s2v(indentStx), 9)).succeeded } expecting (false) comment "cnl9-4"

currentIndentation := 2
test { lineBreak("right").parse(stringInputStream(s2v(indentStx), 4)).succeeded } expecting (false) comment "cnl4-5"
test { lineBreak("right").parse(stringInputStream(s2v(indentStx), 9)).succeeded } expecting (false) comment "cnl9-5"


currentIndentation := 0
test { lineBreak("same").parse(stringInputStream(s2v(indentStx), 4)).succeeded } expecting (false) comment "cnl4-6"
test { lineBreak("same").parse(stringInputStream(s2v(indentStx), 9)).succeeded } expecting (true) comment "cnl9-6"

currentIndentation := 1
test { lineBreak("same").parse(stringInputStream(s2v(indentStx), 4)).succeeded } expecting (false) comment "cnl4-7"
test { lineBreak("same").parse(stringInputStream(s2v(indentStx), 9)).succeeded } expecting (false) comment "cnl9-7"

currentIndentation := 2
test { lineBreak("same").parse(stringInputStream(s2v(indentStx), 4)).succeeded } expecting (true) comment "cnl4-8"
test { lineBreak("same").parse(stringInputStream(s2v(indentStx), 9)).succeeded } expecting (false) comment "cnl9-8"

currentIndentation := 0




    
def hello = (tokenParser("Hello"))
def dsp   = digitStringParser
def ini   = sequentialParser(
                graceIdentifierParser,
                sequentialParser(
                        whiteSpaceParser,
                        graceIdentifierParser))
def ini2  = (graceIdentifierParser) ~ 
                        (whiteSpaceParser) ~
                                                graceIdentifierParser
def alt   = alternativeParser(hello,dsp)
def alt2  = hello | dsp
def rpx   = repetitionParser(tokenParser("x"))
def rpx2  = rep(tokenParser("x"))
def rpx1  = rep1(tokenParser("x"))
def rs    = repsep(tokenParser("x"),tokenParser("c"))
def r1s   = rep1sep(tokenParser("x"),tokenParser("c"))
def rd    = repdel(tokenParser("x"),tokenParser("c"))
//////////////////////////////////////////////////
// test!

test {strm.take(5)} 
    expecting "Hello" 
    comment "strm.take(5)"
test {strm.rest(6).take(5)} 
    expecting "World" 
    comment "strm.rest(6).take(5)"
test {tokenParser("Hello").parse(strm).succeeded}
    expecting(true)
    comment "tokenParser(\"Hello\")"
test {tokenParser("Hellx").parse(strm).succeeded}
    expecting(false)
    comment "tokenParser(\"Hellx\")"
test {whiteSpaceParser.parse(strm).succeeded}
    expecting(false)
    comment "whiteSpaceParser"
test {whiteSpaceParser.parse(strm2).succeeded}
    expecting(true)
    comment "whiteSpaceParser"
test {whiteSpaceParser.parse(strm2).next.position}
    expecting(4)
    comment "whiteSpaceParser - eating 4"
test {isletter "A"} expecting (true) comment "isletter A"
test {isletter "F"} expecting (true) comment "isletter F"
test {isletter "Z"} expecting (true) comment "isletter Z"
test {isletter "a"} expecting (true) comment "isletter a"
test {isletter "f"} expecting (true) comment "isletter f"
test {isletter "z"} expecting (true) comment "isletter z"
test {isletter "$"} expecting (false) comment "isletter $"
test {isletter "0"} expecting (false) comment "isletter 0"
test {isletter "1"} expecting (false) comment "isletter 1"
test {isletter "9"} expecting (false) comment "isletter 9"
test {isdigit "A"} expecting (false) comment "isdigit A"
test {isdigit "F"} expecting (false) comment "isdigit F"
test {isdigit "Z"} expecting (false) comment "isdigit A"
test {isdigit "a"} expecting (false) comment "isdigit a"
test {isdigit "f"} expecting (false) comment "isdigit f"
test {isdigit "z"} expecting (false) comment "isdigit z"
test {isdigit "$"} expecting (false) comment "isdigit $"
test {isdigit "0"} expecting (true) comment "isdigit 0"
test {isdigit "1"} expecting (true) comment "isdigit 1"
test {isdigit "9"} expecting (true) comment "isdigit 9"
test {whiteSpaceParser.parse(strm2).next.position}
    expecting(4)
    comment "whiteSpaceParser - eating 4"
test {graceIdentifierParser.parse(strmus).next.position}
    expecting(2)
    comment "graceIdentifierParser  us - eating 2"
test {graceIdentifierParser.parse(strmus).succeeded}
    expecting(true)
    comment "graceIdentifierParser us OK"
test {graceIdentifierParser.parse(strmus).result}
    expecting("_")
    comment "graceIdentifierParser. us _"
test {graceIdentifierParser.parse(strmab).next.position}
    expecting(12)
    comment "graceIdentifierParser ab12 "
test {graceIdentifierParser.parse(strmab).succeeded}
    expecting(true)
    comment "graceIdentifierParser ab OK"
test {graceIdentifierParser.parse(strmab).result}
    expecting("abc4de'a123")
    comment "graceIdentifierParser.ab - eating 2"
test {graceIdentifierParser.parse(strmas).next.position}
    expecting(2)
    comment "graceIdentifierParser as pos"
test {graceIdentifierParser.parse(strmas).succeeded}
    expecting(true)
    comment "graceIdentifierParser as"
test {graceIdentifierParser.parse(strmas).result}
    expecting("a")
    comment "graceIdentifierParser as OK"
test {graceIdentifierParser.parse(strmnn).succeeded}
    expecting(false)
    comment "graceIdentifierParser nn - eating 1"
test {digitStringParser.parse(strmnn).next.position}
    expecting(5)
    comment "digitStringParser as pos"
test {digitStringParser.parse(strmnn).succeeded}
    expecting(true)
    comment "digitStringParser as"
test {digitStringParser.parse(strmnn).result}
    expecting("1234")
    comment "digitStringParser as OK"
test {digitStringParser.parse(strmnx).next.position}
    expecting(5)
    comment "digitStringParser as pos"
test {digitStringParser.parse(strmnx).succeeded}
    expecting(true)
    comment "digitStringParser as"
test {digitStringParser.parse(strmnx).result}
    expecting("1234")
    comment "digitStringParser as OK"
test {sequentialParser(ws,hello).parse(strm2).succeeded}
    expecting(true)
    comment "sequentialParser strm2 OK"    
test {sequentialParser(ws,hello).parse(strm).succeeded}
    expecting(false)
    comment "sequentialParser strm OK"    
test {sequentialParser(ws,hello).parse(strmab).succeeded}
    expecting(false)
    comment "sequentialParser strm3 OK"    
test {ini.parse(strmas).succeeded}
    expecting(true)
    comment "sequentialParser ini OK"    
test {ini.parse(strmas).result}
    expecting("a bcb")
    comment "sequentialParser a bcb OK"    
test {sequentialParser(ws,hello).parse(strm2).succeeded}
    expecting(true)
    comment "sequentialParser strm2 OK"    
test {(ws ~ hello).parse(strm2).succeeded}
    expecting(true)
    comment "sequentialParser strm2 OK"    
test {ini2.parse(strmas).succeeded}
    expecting(true)
    comment "sequentialParser ini2 OK"    
test {ini2.parse(strmas).result}
    expecting("a bcb")
    comment "sequentialParser a bcb2 OK"    
test {opt(hello).parse(strm).succeeded}
    expecting(true)
    comment "optionalParser opt(hello) OK"    
test {opt(hello).parse(strmab).succeeded}
    expecting(true)
    comment "optionalParser opt(hello) abOK"    
test {alt.parse(strm).succeeded}
    expecting(true)
    comment "alt Hello OK"    
test {alt.parse(strmnn).succeeded}
    expecting(true)
    comment "alt nn OK"    
test {alt2.parse(strm).succeeded}
    expecting(true)
    comment "alt2 Hello OK"    
test {alt2.parse(strmnn).succeeded}
    expecting(true)
    comment "alt2 nn OK"
test {rpx.parse(strm).succeeded}
    expecting(true)
    comment "rpx Hello OK"    
test {rpx.parse(strmxx).succeeded}
    expecting(true)
    comment "rpx xx OK"    
test {rpx.parse(strmxx).result}
    expecting("xxxxx")
    comment "rpx xxxxx OK"    
test {rpx2.parse(strm).succeeded}
    expecting(true)
    comment "rpx2 Hello OK"    
test {rpx2.parse(strmxx).succeeded}
    expecting(true)
    comment "rpx2 xx OK"    
test {rpx2.parse(strmxx).result}
    expecting("xxxxx")
    comment "rpx2 xxxxx OK"    
test {rpx1.parse(strm).succeeded}
    expecting(false)
    comment "rpx1 Hello OK"    
test {rpx1.parse(strmxx).succeeded}
    expecting(true)
    comment "rpx1 xx OK"    
test {rpx1.parse(strmxx).result}
    expecting("xxxxx")
    comment "rpx1 xxxxx OK"    
test {rpx1.parse(strmxx).next.atEnd}
    expecting(true)
    comment "rpx1 atEnd OK"    
test {dropParser(hello).parse(strm).succeeded}
    expecting(true)
    comment "dropParser(\"Hello\")"
test {dropParser(hello).parse(strm).result}
    expecting("")
    comment "dropParser(\"Hello\") result"
test {dropParser(tokenParser("Hellx")).parse(strm).succeeded}
    expecting(false)
    comment "dropParser(tokenParser(\"Hellx\"))"
test {drop(hello).parse(strm).result}
    expecting("")
    comment "drop(hello) result"
test {trim(hello).parse(strm2).succeeded}
     expecting(true)
     comment "trim(hello) result"
test {trim(hello).parse(strm2).next.position}
     expecting(10) 
     comment "trim(hello) next"
test {trim(symbol("Hello")).parse(strm2).result}
     expecting("Hello")
     comment "trim(symbol(hello)) (not taking trailing space)"
test {rs.parse(strmxc).succeeded}
     expecting(true)
     comment "rs xc"
test {rs.parse(strmxc).next.position}
     expecting(6)
     comment "rs xc p   "
test {rs.parse(strmnn).succeeded}
     expecting(true)
     comment "rs nn"
test {rs.parse(strmnn).next.position}
     expecting(1)
     comment "rs nn p"
test {rs.parse(strmxcx).succeeded}
     expecting(true)
     comment "rs xcx"
test {rs.parse(strmxcx).next.position}
     expecting(4)
     comment "rs xcx p"
test {r1s.parse(strmx).succeeded}
     expecting(true)
     comment "r1s x f"
test {r1s.parse(strmx).next.position}
     expecting(2)
     comment "r1s x f"
test {r1s.parse(strmxc).succeeded}
     expecting(true)
     comment "r1s xc"
test {r1s.parse(strmxc).next.position}
     expecting(6)
     comment "r1s xc p"
test {r1s.parse(strmx).succeeded}
     expecting(true)
     comment "r1s x f"
test {r1s.parse(strmx).next.position}
     expecting(2)
     comment "r1s x f"
test {r1s.parse(strmnn).succeeded}
     expecting(false)
     comment "r1s nn"
test {rd.parse(strmxc).succeeded}
     expecting(true)
     comment "rd xc"
test {rd.parse(strmxc).next.position}
     expecting(6)
     comment "rd xc p   "
test {rd.parse(strmnn).succeeded}
     expecting(true)
     comment "rd nn"
test {rd.parse(strmnn).next.position}
     expecting(1)
     comment "rd nn p"
test {rd.parse(strmcx).succeeded}
     expecting(true)
     comment "rd cx"
test {rd.parse(strmcx).next.position}
     expecting(5)
     comment "rd cx p   "
test {rs.parse(strmcx).succeeded}
     expecting(true)
     comment "rs cx"
test {rs.parse(strmcx).next.position}
     expecting(4)
     comment "rs cx p   "
test {rule {tokenParser("Hello")}.parse(strm).succeeded}
    expecting(true)
    comment "rule tokenParser(\"Hello\")"
test {rule {tokenParser("Hellx")}.parse(strm).succeeded}
    expecting(false)
    comment "rule tokenParser(\"Hellx\")"
test {atEndParser.parse(rpx1.parse(strmxx).next).succeeded}
    expecting(true)
    comment "atEnd OK"    
test {atEndParser.parse(strmxx).succeeded}
    expecting(false)
    comment "not atEnd OK"    
test {characterSetParser("Helo Wrd").parse(strm).succeeded}
    expecting(true)
    comment "CSP OK"    
test {rep(characterSetParser("Helo Wrd")).parse(strm).next.position}
    expecting(12)
    comment "CSP next OK"    
test (not(hello)) on "Hello" wrongly "not(hello)"
test (not(hello)) on "Bood" correctly "not(hello)"
test (not(not(hello))) on "Hello" correctly "not(not(hello)) Hello"
test (both(hello,dsp)) on "Hello" wrongly "both1"
test (both(hello,hello)) on "Hello" correctly "both2"
test (both(hello,not(dsp))) on "Hello" correctly "both3"
test (empty) on "Hello" correctly "empty1"
test (empty) on "12345" correctly "empty2"
test (empty) on "" correctly "empty3"
test (empty ~ hello) on "Hello" correctly "e~h"
test (hello ~ empty) on "Hello" correctly "h~e"
test (hello | empty) on "Hello" correctly "h|e H"
test (empty | hello) on "Hello" correctly "e|h H"
test (hello | empty) on "  " correctly "h|e ws"
test (empty | hello) on "  " correctly "h|e ws"
test (guard(dsp,{ s -> true})) on "1234" correctly "guard t"
test (guard(dsp,{ s -> false})) on "1234" wrongly "guard f"
test (guard(dsp, { s -> s == "1234" } )) on "1234" correctly "guard 1234"
test (guard(dsp, { s -> s == "1234" } )) on "1235" wrongly "guard f"

test (characterSetNotParser("\n")) on "\nabc" wrongly "cSNP"
test (characterSetNotParser("\n")) on "abc" correctly "cSNP"
 
print "------: done combinator tests"










print  "done long.grace"
