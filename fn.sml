
(*Functional Stack Structure*)
signature STACK =
sig
    type 'a Stack
    exception EmptyStack
    exception Error of string
    val create: 'a Stack
    val push : 'a * 'a Stack -> 'a Stack
    val pop : 'a Stack -> 'a Stack
    val top : 'a Stack -> 'a
    val empty: 'a Stack -> bool
    val poptop : 'a stack -> ('a * 'a stack) option
    val nth : 'a stack * int -> 'a
    val drop : 'a stack * int -> 'a stack
    val depth : 'a stack -> int
    val app : ('a -> unit) -> 'a stack -> unit
    val map : ('a -> 'b) -> 'a stack -> 'b stack
    val mapPartial : ('a -> 'b option) -> 'a stack -> 'b stack
    val find : ('a -> bool) -> 'a stack -> 'a option
    val filter : ('a -> bool) -> 'a stack -> 'a stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b
    val exists : ('a -> bool) -> 'a stack -> bool
    val all : ('a -> bool) -> 'a stack -> bool
    val list2stack : 'a list -> 'a stack 
    val stack2list: 'a stack -> 'a list 
    val toString: ('a -> string) -> 'a stack -> string
end


(*Stack is implemented using sml list, and its inbuilt functions are used in the process*)
structure FunStack : STACK =
struct
  type 'a Stack = 'a list
  exception EmptyStack
  exception Error of string
  val create = [];
  fun push(x,stk) = x::stk;
  fun pop(stk) = tl(stk);
  fun top(stk) = hd(stk);
  fun empty(stk) = if (length(stk) <> 0) then false else true;
  fun poptop(stk) = if (length(stk) = 0) then NONE else (SOME(hd(stk),tl(stk)));
  fun nth(stk,n) = List.nth(stk,n);
  fun drop(stk,n) = List.drop(stk,n);
  fun depth(stk) = List.length(stk);
  fun app f stk = List.app f stk;
  fun map f stk = List.map f stk;
  fun mapPartial f stk = List.mapPartial f stk;
  fun find f stk = List.find f stk;
  fun filter f stk = List.filter f stk;
  fun foldr f i stk = List.foldr f i stk;
  fun foldl f i stk = List.foldl f i stk;
  fun exists f stk = List.exists f stk;
  fun all f stk = List.all f stk;
  fun list2stack(l) = l;
  fun stack2list(s) = s;
  fun toString f stack = foldr op ^ "" (map f stack);
end



(*Converts Node(_,_,_,_) expressions to tuples of strings in the postfix order*)
fun topostfix (INTNODE (NUMERAL)) = (NUMERAL, EMPTY, EMPTY,EMPTY)
  | topostfix (STRINGNODE (VARIABLE)) = (VARIABLE, EMPTY, EMPTY,EMPTY)
  | topostfix (NODE (TT, EMPTY, EMPTY, EMPTY)) = (EMPTY,EMPTY, EMPTY,"TT")
  | topostfix (NODE (FF, EMPTY, EMPTY, EMPTY)) = (EMPTY,EMPTY, EMPTY,"FF")
  | topostfix (NODE (NEG, EXPRESSION, EMPTY, EMPTY)) = (toPostFix(EXPRESSION), EMPTY, EMPTY , "NEG")
  | topostfix (NODE (NOT, EXPRESSION, EMPTY, EMPTY)) = (toPostFix(EXPRESSION), EMPTY, EMPTY , "NOT")
  | topostfix (NODE (OR, EXPRESSION1,EXPRESSION2, EMPTY)) = (toPostFix(EXPRESSION1) , toPostFix(EXPRESSION2), EMPTY , "OR")
  | topostfix (NODE (GT, EXPRESSION1,EXPRESSION2, EMPTY)) = (toPostFix(EXPRESSION1) , toPostFix(EXPRESSION2), EMPTY, "GT")
  | topostfix (NODE (LT, EXPRESSION1,EXPRESSION2, EMPTY)) = (toPostFix(EXPRESSION1) , toPostFix(EXPRESSION2), EMPTY, "LT")
  | topostfix (NODE (EQ, EXPRESSION1,EXPRESSION2, EMPTY)) = (toPostFix(EXPRESSION1) , toPostFix(EXPRESSION2), EMPTY, "EQ")
  | topostfix (NODE (NEQ, EXPRESSION1,EXPRESSION2, EMPTY)) = (toPostFix(EXPRESSION1) , toPostFix(EXPRESSION2), EMPTY , "NEQ")
  | topostfix (NODE (AND, EXPRESSION1,EXPRESSION2, EMPTY)) = (toPostFix(EXPRESSION1) , toPostFix(EXPRESSION2), EMPTY , "AND")
  | topostfix (NODE (DIV, EXPRESSION1,EXPRESSION2, EMPTY)) = (toPostFix(EXPRESSION1) , toPostFix(EXPRESSION2), EMPTY , "DIV")
  | topostfix (NODE (MOD, EXPRESSION1,EXPRESSION2, EMPTY)) = (toPostFix(EXPRESSION1) , toPostFix(EXPRESSION2), EMPTY , "MOD")
  | topostfix (NODE (LEQ, EXPRESSION1,EXPRESSION2, EMPTY)) = (toPostFix(EXPRESSION1) , toPostFix(EXPRESSION2), EMPTY , "LEQ")
  | topostfix (NODE (GEQ, EXPRESSION1,EXPRESSION2, EMPTY)) = (toPostFix(EXPRESSION1) , toPostFix(EXPRESSION2), EMPTY , "GEQ")
  | topostfix (NODE (PLUS, EXPRESSION1,EXPRESSION2, EMPTY)) = (toPostFix(EXPRESSION1) , toPostFix(EXPRESSION2), EMPTY , "PLUS")
  | topostfix (NODE (MINUS, EXPRESSION1,EXPRESSION2, EMPTY)) = (toPostFix(EXPRESSION1) , toPostFix(EXPRESSION2), EMPTY , "MINUS")
  | topostfix (NODE (TIMES, EXPRESSION1,EXPRESSION2, EMPTY)) = (toPostFix(EXPRESSION1) , toPostFix(EXPRESSION2), EMPTY , "TIMES");


(* main "postfix" function that translates the Block of commands into post-fix order*)
fun postfix (NODE(WH,EXPRESSION,COMMANDSEQ, EMPTY)) = (toPostExp(EXPRESSION), toPostCmdSeq(COMMANDSEQ), EMPTY, "WH")
  | postfix (NODE(SET,STRINGNODE(VARIABLE),EXPRESSION, EMPTY)) = (toPostExp(EXPRESSION) , toPostCmdSeq(COMMANDSEQ), EMPTY , "SET")
  | postfix (NODE(ITE,EXPRESSION,COMMANDSEQ1,COMMANDSEQ2)) = (toPostExp(EXPRESSION) , toPostCmdSeq(COMMANDSEQ1) , toPostCmdSeq(COMMANDSEQ2) , "ITE")
  | postfix (NODE(READ,STRINGNODE(VARIABLE), EMPTY, EMPTY)) = (STRINGNODE(VARIABLE), EMPTY, EMPTY , "READ")
  | postfix (NODE(WRITE,EXPRESSION, EMPTY, EMPTY)) = (toPostExp(EXPRESSION), EMPTY , EMPTY , "WRITE")
(*circular helper function*)
and postfixSeq (NODE(SEQ,COMMAND,COMMANDSEQ, EMPTY)) = (toPostCmd(COMMAND),toPostCmdSeq(COMMANDSEQ),EMPTY,"SEQ")
  | postfixSeq (EMPTY) = (EMPTY, EMPTY , EMPTY , "");
  


