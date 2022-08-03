structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val row = ref 1
  val column = ref 2
  val eof = fn () => Tokens.EOF(!row, !column)
  val error = fn (e, lineNo:int, _) => TextIO.output(TextIO.stdOut,"line no: " ^ (Int.toString lineNo) ^ " : " ^ e ^ "\n")
  
%%
%header (functor ParsLexFun(structure Tokens:Pars_TOKENS));

alphabet=[A-Za-z];
numbers=[0-9];
blank = [\ \t];
numeral= [0-9]+;

%%
[\n]  => (column := 1; row := (!row) + 1; lex());
[\r]  => (column := 1; row := (!row) + 1; lex());
"+"       => (column := (!column) + 1; Tokens.PLUS(!row,!column));
"-"       => (column := (!column) + 1; Tokens.MINUS(!row,!column));
"*"       => (column := (!column) + 1; Tokens.TIMES(!row,!column));
"/"       => (column := (!column) + 1; Tokens.DIV(!row,!column));
"%"       => (column := (!column) + 1; Tokens.MOD(!row,!column));
"&&"      => (column := (!column) + 2; Tokens.AND(!row,!column));
"||"      => (column := (!column) + 2; Tokens.OR(!row,!column));
":="      => (column := (!column) + 2; Tokens.SET(!row,!column));
">"       => (column := (!column) + 1; Tokens.GT(!row,!column));
">="      => (column := (!column) + 2; Tokens.GEQ(!row,!column));
"<>"      => (column := (!column) + 2; Tokens.NEQ(!row,!column));
"!"       => (column := (!column) + 1; Tokens.NOT(!row,!column));
"~"       => (column := (!column) + 1; Tokens.NEG(!row,!column));
"<"       => (column := (!column) + 1; Tokens.LT(!row,!column));
"<="      => (column := (!column) + 2; Tokens.LEQ(!row,!column));
"="       => (column := (!column) + 1; Tokens.EQ(!row,!column));
"{"       => (column := (!column) + 1; Tokens.LBRACE(!row,!column));
"}"       => (column := (!column) + 1; Tokens.RBRACE(!row,!column));
";"       => (column := (!column) + 1; Tokens.SEMICOLON (!row,!column));
","       => (column := (!column) + 1; Tokens.COMMA (!row,!column));
":"       => (column := (!column) + 1; Tokens.COLON (!row,!column));
"::"      => (column := (!column) + 2; Tokens.DCOLON (!row,!column));
"("       => (column := (!column) + 1; Tokens.LPAREN(!row,!column));
")"       => (column := (!column) + 1; Tokens.RPAREN(!row,!column));
"tt"      => (column := (!column) + 2; Tokens.TT(!row,!column));
"ff"      => (column := (!column) + 2; Tokens.FF(!row,!column));
"if"      => (column := (!column) + 2; Tokens.IF(!row,!column));
"do"      => (column := (!column) + 2; Tokens.DO(!row,!column));
"int"     => (column := (!column) + 3; Tokens.INT(!row,!column));
"var"     => (column := (!column) + 3; Tokens.VAR(!row,!column));
"then"    => (column := (!column) + 4; Tokens.THEN(!row,!column));
"read"    => (column := (!column) + 4; Tokens.READ(!row,!column));
"write"   => (column := (!column) + 5; Tokens.WRITE(!row,!column));
"program" => (column := (!column) + 7; Tokens.PROG(!row,!column));
"endwh"   => (column := (!column) + 5; Tokens.ENDWH(!row,!column));
"bool"    => (column := (!column) + 4; Tokens.BOOL(!row,!column));
"else"    => (column := (!column) + 4; Tokens.ELSE(!row,!column));
"endif"   => (column := (!column) + 5; Tokens.ENDIF(!row,!column));
"while"   => (column := (!column) + 5; Tokens.WH(!row,!column));
{alphabet}+  => (column := (!column) + size yytext; Tokens.VARIABLE(yytext,!row,!column));
{numeral} => (column := (!column) + size yytext; Tokens.NUMERAL(valOf(Int.fromString(yytext)),!row,!column));
{blank}+     => (column := (!column) + size yytext ;lex());
