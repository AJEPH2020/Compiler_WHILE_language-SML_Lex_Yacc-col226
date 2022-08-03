CM.make("$/basis.cm");
(*CM.make("$smlnj/compile/compile.cm");*)
(*CM.make("$/smlnj-lib.cm");*)
CM.make("$/ml-yacc-lib.cm");
use "par.yacc.sig";
use "par.yacc.sml";
use "lex.lex.sml";
use "toASTCodeRunner.sml";
(*use "fn.sml";*)
Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;