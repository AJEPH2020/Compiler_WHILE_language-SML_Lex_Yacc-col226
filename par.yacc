(*DataTypes*)
datatype nod= LT | LEQ | GEQ | GT | EQ | NEQ | INT of string | BOOL of string | PROG | BLK | DE | SET | SEQ | AND | OR | NOT | TT | ITE | MOD | DIV | TIMES | MINUS | PLUS | NEG | FF | WH | READ | WRITE;
datatype Tree= EMPTY | INTNODE of int | STRINGNODE of string | NODE of nod*Tree*Tree*Tree;
datatype varType= BOOLEAN | INTEGER ;

(*Decleration Function*)
fun mkTree(BOOLEAN,head::tail,z)=NODE(DE,NODE(SEQ,NODE(BOOL(head),EMPTY,EMPTY, EMPTY),mkTree(BOOLEAN,tail,EMPTY), EMPTY),z, EMPTY)
  | mkTree(INTEGER,head::tail,z)=NODE(DE,NODE(SEQ,NODE(INT(head),EMPTY,EMPTY, EMPTY),mkTree(INTEGER,tail,EMPTY), EMPTY),z, EMPTY)
  | mkTree(y,[],z)=EMPTY;

exception UndefinedType;
exception UnspecifiedVariable;

fun stEq(x, y) =
if x <> y then false else true;


(*Memory array of the VMC machine in the form of HashTable*)
val mapTable : (string, int) HashTable.hash_table = HashTable.mkTable (HashString.hashString, stEq) (101, UnspecifiedVariable);


(*
(***These functions are not working correctly but as I've spend time making these, so just leaving these as comments***)

(*Function to fill the Memory array/hashtable*)
fun hashFill(type,vl,dec) =
if(vl = []) then EMPTY
else if(type = "bool") then
let
 val a = HashTable.insert mapTable (hd(vl), 1);
 val b = hashFill(type,tl(vl),dec)
in
mkTree(type,vl,dec)
end
else if(type = "int") then
let
 val a = HashTable.insert mapTable (hd(vl), 0);
 val b = hashFill(type,tl(vl),dec)
in
mkTree(type,vl,dec)
end
else raise UndefinedType;


(*Function for type checking*)
fun type(NODE(NEG, EXPRESSION, EMPTY, EMPTY))            = if(type(EXPRESSION)=0)                                    then 0                        else raise UndefinedType
    | type(NODE(PLUS, EXPRESSION1, EXPRESSION2, EMPTY) ) = if(type(EXPRESSION1)=0 andalso type(EXPRESSION2)=0)       then 0                        else raise UndefinedType
    | type(NODE(MINUS, EXPRESSION1, EXPRESSION2, EMPTY)) = if(type(EXPRESSION1)=0 andalso type(EXPRESSION2)=0)       then 0                        else raise UndefinedType
    | type(NODE(TIMES, EXPRESSION1, EXPRESSION2, EMPTY)) = if(type(EXPRESSION1)=0 andalso type(EXPRESSION2)=0)       then 0                        else raise UndefinedType
    | type(NODE(DIV, EXPRESSION1, EXPRESSION2, EMPTY))   = if(type(EXPRESSION1)=0 andalso type(EXPRESSION2)=0)       then 0                        else raise UndefinedType
    | type(NODE(MOD, EXPRESSION1, EXPRESSION2, EMPTY))   = if(type(EXPRESSION1)=0 andalso type(EXPRESSION2)=0)       then 0                        else raise UndefinedType
    | type(NODE(NOT, EXPRESSION, EMPTY, EMPTY))          = if(type(EXPRESSION)=1)                                    then 1                        else raise UndefinedType
    | type(NODE(OR, EXPRESSION1,EXPRESSION2, EMPTY))     = if(type(EXPRESSION1)=1 andalso type(EXPRESSION2)=1)       then 1                        else raise UndefinedType
    | type(NODE(AND, EXPRESSION1, EXPRESSION2, EMPTY))   = if(type(EXPRESSION1)=1 andalso type(EXPRESSION2)=1)       then 1                        else raise UndefinedType
    | type(NODE(LT, EXPRESSION1, EXPRESSION2, EMPTY))    = if(type(EXPRESSION1) = type(EXPRESSION2))                 then 1                        else raise UndefinedType
    | type(NODE(LEQ, EXPRESSION1, EXPRESSION2, EMPTY))   = if(type(EXPRESSION1) = type(EXPRESSION2))                 then 1                        else raise UndefinedType    
    | type(NODE(EQ, EXPRESSION1, EXPRESSION2, EMPTY))    = if(type(EXPRESSION1) = type(EXPRESSION2))                 then 1                        else raise UndefinedType
    | type(NODE(GT, EXPRESSION1, EXPRESSION2, EMPTY))    = if(type(EXPRESSION1) = type(EXPRESSION2))                 then 1                        else raise UndefinedType
    | type(NODE(GEQ, EXPRESSION1, EXPRESSION2, EMPTY))   = if(type(EXPRESSION1) = type(EXPRESSION2))                 then 1                        else raise UndefinedType
    | type(NODE(NEQ, EXPRESSION1, EXPRESSION2, EMPTY))   = if(type(EXPRESSION1) = type(EXPRESSION2))                 then 1                        else raise UndefinedType
    | type(STRINGNODE(VARIABLE))                         = variableType(VARIABLE)
    | type(INTNODE(NUMERAL))                             = 0
    | type(NODE(TT, EMPTY, EMPTY, EMPTY))                = 1
    | type(NODE(FF, EMPTY, EMPTY, EMPTY))                = 1
and variableType(VARIABLE)                                = HashTable.lookup mapTable VARIABLE;
fun typcSET(SET,STRINGNODE(VARIABLE),EXPRESSION, EMPTY)  = if(variableType(VARIABLE) = type(EXPRESSION))             then NODE (SET,STRINGNODE(VARIABLE),EXPRESSION, EMPTY)    else raise UndefinedType;
fun typcWRITE(WRITE,EXPRESSION, EMPTY, EMPTY)            = if(type(EXPRESSION)  = 0)                                 then NODE (WRITE,EXPRESSION, EMPTY, EMPTY)                else raise UndefinedType;
fun typcITE(ITE,EXPRESSION,COMMANDSEQ1,COMMANDSEQ2)      = if(type(EXPRESSION)  = 1)                                 then NODE (ITE,EXPRESSION,COMMANDSEQ1,COMMANDSEQ2)        else raise UndefinedType;
fun typcWH(WH,EXPRESSION,COMMANDSEQ, EMPTY)              = if(type(EXPRESSION)  = 1)                                 then NODE (WH,EXPRESSION,COMMANDSEQ, EMPTY)               else raise UndefinedType;
fun typcNEG(NEG, EXPRESSION, EMPTY, EMPTY)               = if(type(EXPRESSION)  = 0)                                 then NODE (NEG, EXPRESSION, EMPTY, EMPTY)                 else raise UndefinedType;
fun typcPLUS(PLUS, EXPRESSION1, EXPRESSION2, EMPTY)      = if(type(EXPRESSION1) = 0 andalso type(EXPRESSION2) = 0)   then NODE (PLUS, EXPRESSION1, EXPRESSION2, EMPTY)         else raise UndefinedType;
fun typcMINUS(MINUS, EXPRESSION1, EXPRESSION2, EMPTY)    = if(type(EXPRESSION1) = 0 andalso type(EXPRESSION2) = 0)   then NODE (MINUS, EXPRESSION1, EXPRESSION2, EMPTY)        else raise UndefinedType;
fun typcTIMES(TIMES, EXPRESSION1, EXPRESSION2, EMPTY)    = if(type(EXPRESSION1) = 0 andalso type(EXPRESSION2) = 0)   then NODE (TIMES, EXPRESSION1, EXPRESSION2, EMPTY)        else raise UndefinedType;
fun typcDIV(DIV, EXPRESSION1, EXPRESSION2, EMPTY)        = if(type(EXPRESSION1) = 0 andalso type(EXPRESSION2) = 0)   then NODE (DIV, EXPRESSION1, EXPRESSION2, EMPTY)          else raise UndefinedType;
fun typcMOD(MOD, EXPRESSION1, EXPRESSION2, EMPTY)        = if(type(EXPRESSION1) = 0 andalso type(EXPRESSION2) = 0)   then NODE (MOD, EXPRESSION1, EXPRESSION2, EMPTY)          else raise UndefinedType;
fun typcNOT(NOT, EXPRESSION, EMPTY, EMPTY)               = if(type(EXPRESSION)  = 1)                                 then NODE (NOT, EXPRESSION, EMPTY, EMPTY)                 else raise UndefinedType;
fun typcOR(OR, EXPRESSION1,EXPRESSION2, EMPTY)           = if(type(EXPRESSION1) = 1 andalso type(EXPRESSION2) = 1)   then NODE (OR, EXPRESSION1,EXPRESSION2, EMPTY)            else raise UndefinedType;
fun typcAND(AND, EXPRESSION1, EXPRESSION2, EMPTY)        = if(type(EXPRESSION1) = 1 andalso type(EXPRESSION2) = 1)   then NODE (AND, EXPRESSION1, EXPRESSION2, EMPTY)          else raise UndefinedType;
fun typcLT(LT, EXPRESSION1, EXPRESSION2, EMPTY)          = if(type(EXPRESSION1) = type(EXPRESSION2))                 then NODE (LT, EXPRESSION1, EXPRESSION2, EMPTY)           else raise UndefinedType;
fun typcLEQ(LEQ, EXPRESSION1, EXPRESSION2, EMPTY)        = if(type(EXPRESSION1) = type(EXPRESSION2))                 then NODE (LEQ, EXPRESSION1, EXPRESSION2, EMPTY)          else raise UndefinedType;
fun typcGT(GT, EXPRESSION1, EXPRESSION2, EMPTY)          = if(type(EXPRESSION1) = type(EXPRESSION2))                 then NODE (GT, EXPRESSION1, EXPRESSION2, EMPTY)           else raise UndefinedType;
fun typcGEQ(GEQ, EXPRESSION1, EXPRESSION2, EMPTY)        = if(type(EXPRESSION1) = type(EXPRESSION2))                 then NODE (GEQ, EXPRESSION1, EXPRESSION2, EMPTY)          else raise UndefinedType;
fun typcEQ(EQ, EXPRESSION1, EXPRESSION2, EMPTY)          = if(type(EXPRESSION1) = type(EXPRESSION2))                 then NODE (EQ, EXPRESSION1, EXPRESSION2, EMPTY)           else raise UndefinedType;
fun typcNEQ(NEQ, EXPRESSION1, EXPRESSION2, EMPTY)        = if(type(EXPRESSION1) = type(EXPRESSION2))                 then NODE (NEQ, EXPRESSION1, EXPRESSION2, EMPTY)          else raise UndefinedType;
*)
%%

%name Pars

%term
  TT | FF | WH | DO | ENDWH | GT | GEQ | NEQ | AND | OR | SEMICOLON | COMMA | LBRACE | RBRACE | NEG | EOF | NOT | PLUS | MINUS | TIMES | DIV | MOD | SET | IF | THEN
  | RPAREN | LPAREN | NUMERAL of int | LT | LEQ | EQ | COLON | DCOLON | ELSE | ENDIF | PROG | VAR | INT | BOOL | VARIABLE of string | READ | WRITE


%nonterm   BLOCK of Tree | TYPE of varType | VARLIST of string list | PROGRAM of Tree | COMMAND of Tree | EXPRESSION of Tree | COMMANDSEQ of Tree | DECLARATION of Tree 
%pos int

%noshift EOF
%eop EOF
%left MINUS
%left TIMES
%left AND
%left OR
%left PLUS
%left EQ NEQ
%left MOD DIV
%left GT LT GEQ LEQ
%right NOT
%right NEG
%start PROGRAM

%verbose

%%

PROGRAM : PROG VARIABLE DCOLON BLOCK                                                  (NODE(PROG,STRINGNODE(VARIABLE),BLOCK, EMPTY))
BLOCK : DECLARATION LBRACE COMMANDSEQ RBRACE                                          (NODE(BLK,DECLARATION,COMMANDSEQ, EMPTY))
TYPE :BOOL                                                                            (BOOLEAN)
 | INT                                                                                (INTEGER)
VARLIST : VARIABLE COLON                                                              ([VARIABLE])
 | VARIABLE COMMA VARLIST                                                             (VARIABLE::VARLIST)
DECLARATION : VAR VARLIST TYPE SEMICOLON DECLARATION                                  (mkTree(TYPE,VARLIST,DECLARATION))
 |                                                                                    (EMPTY)
COMMAND : WH EXPRESSION DO LBRACE COMMANDSEQ RBRACE ENDWH                             (NODE(WH,EXPRESSION,COMMANDSEQ, EMPTY))
 | VARIABLE SET EXPRESSION                                                            (NODE(SET,STRINGNODE(VARIABLE),EXPRESSION, EMPTY))
 | IF EXPRESSION THEN LBRACE COMMANDSEQ RBRACE ELSE LBRACE COMMANDSEQ RBRACE ENDIF    (NODE(ITE,EXPRESSION,COMMANDSEQ1,COMMANDSEQ2))
 | READ VARIABLE                                                                      (NODE(READ,STRINGNODE(VARIABLE), EMPTY, EMPTY))
 | WRITE EXPRESSION                                                                   (NODE(WRITE,EXPRESSION, EMPTY, EMPTY))
COMMANDSEQ : COMMAND SEMICOLON COMMANDSEQ                                             (NODE(SEQ,COMMAND,COMMANDSEQ, EMPTY))
 |                                                                                    (EMPTY)
EXPRESSION : LPAREN EXPRESSION RPAREN                                                 (EXPRESSION)
 | NUMERAL                                                                            (INTNODE (NUMERAL))
 | TT                                                                                 (NODE(TT, EMPTY, EMPTY, EMPTY))
 | FF                                                                                 (NODE (FF, EMPTY, EMPTY, EMPTY))
 | VARIABLE                                                                           (STRINGNODE (VARIABLE))
 | NOT EXPRESSION                                                                     (NODE (NOT, EXPRESSION, EMPTY, EMPTY))
 | NEG EXPRESSION                                                                     (NODE (NEG, EXPRESSION, EMPTY, EMPTY))
 | PLUS EXPRESSION                                                                    (NODE (PLUS, EXPRESSION, EMPTY, EMPTY))
 | MINUS EXPRESSION                                                                   (NODE (MINUS, EXPRESSION, EMPTY, EMPTY))
 | EXPRESSION OR EXPRESSION                                                           (NODE (OR, EXPRESSION1,EXPRESSION2, EMPTY))
 | EXPRESSION GT EXPRESSION                                                           (NODE (GT, EXPRESSION1, EXPRESSION2, EMPTY))
 | EXPRESSION LT EXPRESSION                                                           (NODE (LT, EXPRESSION1, EXPRESSION2, EMPTY))
 | EXPRESSION EQ EXPRESSION                                                           (NODE (EQ, EXPRESSION1, EXPRESSION2, EMPTY))
 | EXPRESSION NEQ EXPRESSION                                                          (NODE (NEQ, EXPRESSION1, EXPRESSION2, EMPTY))
 | EXPRESSION AND EXPRESSION                                                          (NODE (AND, EXPRESSION1, EXPRESSION2, EMPTY))
 | EXPRESSION DIV EXPRESSION                                                          (NODE (DIV, EXPRESSION1, EXPRESSION2, EMPTY))
 | EXPRESSION MOD EXPRESSION                                                          (NODE (MOD, EXPRESSION1, EXPRESSION2, EMPTY))
 | EXPRESSION LEQ EXPRESSION                                                          (NODE (LEQ, EXPRESSION1, EXPRESSION2, EMPTY))
 | EXPRESSION GEQ EXPRESSION                                                          (NODE (GEQ, EXPRESSION1, EXPRESSION2, EMPTY))
 | EXPRESSION PLUS EXPRESSION                                                         (NODE (PLUS, EXPRESSION1, EXPRESSION2, EMPTY))
 | EXPRESSION MINUS EXPRESSION                                                        (NODE (MINUS, EXPRESSION1, EXPRESSION2, EMPTY))
 | EXPRESSION TIMES EXPRESSION                                                        (NODE (TIMES, EXPRESSION1, EXPRESSION2, EMPTY))