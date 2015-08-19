module ogol::Syntax

import IO;
import vis::ParseTree;
/*

Ogol syntax summary

Program: Command...

Command:
 * Control flow: 
  if Expr Block
  ifelse Expr Block Block
  while Expr Block
  repeat Expr Block
 * Drawing (mind the closing semicolons)
  forward Expr; fd Expr; back Expr; bk Expr; home;
  right Expr; rt Expr; left Expr; lt Expr; 
  pendown; pd; penup; pu;
 * Procedures
  definition: to Name [Var...] Command... end
  call: Name Expr... ;
 
Block: [Command...]
 
Expressions
 * Variables :x, :y, :angle, etc.
 * Number: 1, 2, -3, 0.7, -.1, etc.
 * Boolean: true, false
 * Arithmetic: +, *, /, -
 * Comparison: >, <, >=, <=, =, !=
 * Logical: &&, ||

Reserved keywords
 if, ifelse, while, repeat, forward, back, right, left, pendown, 
 penup, to, true, false, end

Bonus:
 - add literal for colors
 - support setpencolor

*/


start syntax Program = prg: Command*; 

syntax Procedure = FunDef | FunCall;
syntax FunDef = "to " FunId VarId* Command* "end";
syntax FunCall = FunId Expr* ";" ;

syntax Expr = Bool | Number | VarId |
				left Expr "/" Expr  >
				left Expr "*" Expr >
				left ( Expr "-" Expr | left Expr "+" Expr ) >				
				left ( Expr "\<" Expr | Expr "\<=" Expr | Expr "\>" Expr | Expr "\>=" Expr | Expr "=" Expr | Expr "!=" Expr )			
				 ;

syntax Command = ControlFlow | Procedure | Drawing  /*ControlFlow /* | Drawing | Procedure */;

syntax Block = "["Command*"]";

syntax ControlFlow = "if" Expr Block |
					"ifelse" Expr Block Block |
					"while" Expr Block |
					"repeat" Expr Block;
					
syntax Direction = "forward" | "fd" | "back" | "bk" | "right" | "rt" | "left" | "lt";
syntax Drawing = Direction Expr ";" | "home;";
					
				
lexical Number = "."?[0-9]+ !>>[0-9];
lexical Bool = "true" | "false";

lexical VarId
  = ":" ([a-zA-Z][a-zA-Z0-9]*) \ Reserved !>> [a-zA-Z0-9];
  
lexical FunId
  = ([a-zA-Z][a-zA-Z0-9]*) \ Reserved !>> [a-zA-Z0-9];


layout Standard 
  = WhitespaceOrComment* !>> [\ \t\n\r] !>> "--";
  
lexical WhitespaceOrComment 
  = whitespace: Whitespace
  | comment: Comment
  ; 

lexical Whitespace
  = [\ \t\n\r]
  ;

lexical Comment
  = @category="Comment" "--" ![\n\r]* $
  ;  
  
keyword Reserved = "if" | "ifelse" | "while" |  "repeat" |  "forward" | "back" | "right" | "left" | "pendown" | 
 "penup" | "to" | "true" | "false" | "end" | "home" | "forward" | "fd" | "back" | "bk" | "right" | "rt" | "left" | "lt";

  
public bool Test(t,  str ToTest)
{
	try 
	{
		return /amb(_)!:= parse(t, ToTest);
	}
  	catch x: return false;
}  
  
test bool t01() = Test(#Expr, "5");
test bool t02() = Test(#Expr, ":x");
test bool t03() = Test(#Expr, "true");
test bool t04() = Test(#Expr, "false");
test bool t05() = Test(#Expr, "1*5");
test bool t06() = Test(#Expr, "false/5");
test bool t07() = Test(#Expr, "2*:x");
test bool t08() = Test(#Expr, "2-5-6");


test bool t12() = Test(#Expr, "30");
test bool t13() = Test(#Command, "lt 30;");
test bool t20() = Test(#Program, "lt 30; tree :size*.7;");

 test bool t21() = Test(#Command, "fd :size;");
 test bool t22() = Test(#Block, "[\n\r  \tfd :size;\n\r  \tlt 30; tree :size*.7;\n\r  \trt 60; tree :size*.7;\n\r  \tlt 30; bk :size;\n\r  ]");
 test bool t23() = Test(#Block, "[rt 60; tree :size*.7;]");
 test bool t24() = Test(#Block, "[lt 30; bk :size;]");
 test bool t25() = Test(#FunDef, "to tree :size end");
 test bool t26() = Test(#Command, "if :size \>= 5 [fd :size; lt 30; tree :size*.7; rt 60; tree :size*.7; lt 30; bk :size;]");
 test bool t27() = Test(#FunDef, "to tree :size if :size \>= 5 [fd :size; lt 30; tree :size*.7; rt 60; tree :size*.7; lt 30; bk :size;] end");
test bool t28() = Test(#start[Program], "to tree :size  if :size \>= 5   [  \tfd :size;\n\r  \tlt 30; tree :size*.7;  \trt 60; tree :size*.7;  \tlt 30; bk :size;  ] end");

 //is prg;
 // 
 //public test bool t02() {	Test(#start[Program], "dash")	}
 //public test bool t03() { 	Test(#start[Program], "to dash 5-8 end"); }
 test bool tF01() =  Test(#start[Program],readFile(|file:///Users/Bouke/Documents/Git/ogol/input/test.ogol|));
 test bool tF02() =  Test(#start[Program],readFile(|file:///Users/Bouke/Documents/Git/ogol/input/trees.ogol|));
 test bool tF03() =  Test(#start[Program],readFile(|file:///Users/Bouke/Documents/Git/ogol/input/octagon.ogol|));
 test bool tF04() =  Test(#start[Program],readFile(|file:///Users/Bouke/Documents/Git/ogol/input/dashed.ogol|));
 
public void tx1() { renderParsetree(parse(#Expr, "2 + 3 * 4")); }
public void tx2() { renderParsetree(parse(#Expr, "4 * 3 + 2")); }
 