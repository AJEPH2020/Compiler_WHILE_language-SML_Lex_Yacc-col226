structure ParsLrVals = ParsLrValsFun(structure Token = LrParser.Token);
structure ParsLex = ParsLexFun(structure Tokens = ParsLrVals.Tokens);
structure ParsParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = ParsLrVals.ParserData
     	       structure Lex = ParsLex);
     
fun check (strl) =
    	let fun print_error (s,pos:int,_) =
		    TextIO.output(TextIO.stdOut, "Error: line no: " ^ (Int.toString(pos)) ^ " : " ^ s ^ "\n")
		in
		    ParsParser.parse(0,strl,print_error,())
		end

fun pars (lexr) =
    let val dummyEOF = ParsLrVals.Tokens.EOF(0,0)
    	val (result, lexr) = check lexr
		val (nextToken, lexr) = ParsParser.Stream.get lexr
    in
        if ParsParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

fun lexFromStr (str) =
    let val done = ref false
    	val lexer=  ParsParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
		lexer
    end

fun readFile (infile:string) = 
	let 
		val inputStr = TextIO.openIn infile
		fun loop inputStr = 
			String.implode(String.explode(TextIO.inputAll inputStr))
	in
		loop inputStr before TextIO.closeIn inputStr
	end


val parseOut = pars o lexFromStr o readFile
