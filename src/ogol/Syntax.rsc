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

start syntax Program = Command*; 

syntax Command = ifStat:     "if"     Expr Block       |
				 ifElseStat: "ifelse" Expr Block Block |
				 whileStat:  "while"  Expr Block       |
				 repeatStat: "repeat" Expr Block	   |
				 
				 "forward" Expr ";" | "fd" Expr ";" |
				 "back"    Expr ";" | "bk" Expr ";" |
				 "right"   Expr ";" | "rt" Expr ";" |
				 "left"    Expr ";" | "lt" Expr ";" |
				 "home" ";" |
				 
				 "penup" ";" | "pu" ";" | "pendown" ";" | "pd" ";" |
				 
				 FunDef |
				 FunCall: FunId Expr* ";";

syntax FunDef=   "to" FunId VarId* Command* "end";

syntax Block = "[" Command* "]";

syntax Expr 
   = Bool
   | Num
   | VarId
   > left   div: Expr "/" Expr 
   > left   mul: Expr "*" Expr
   > left ( add: Expr "+" Expr 
   		  | sub: Expr "-" Expr
   		  )
   > left ( gt:  Expr "\>"  Expr
          | st:  Expr "\<"  Expr
          | gte: Expr "\>=" Expr
          | ste: Expr "\<=" Expr
          | eq:  Expr "="  Expr
          | neq: Expr "!=" Expr
          )    
   | left ( and: Expr "&&" Expr
          | or:  Expr "||" Expr
          )
   ;

keyword Reserved = "if" | "ifelse" | "while" | "repeat" |
				   "forward" | "fd" | "back" | "bk" | "right" | "rt" | "left" | "lt" | "home" |
				   "pendown" | "pd" | "penup" | "pu" | "to"  | "true" | "false" | "end";

lexical Bool = "true" | "false";

lexical Num = "-"? ([0-9]* ".")? [0-9]+ !>> [0-9];

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
  
public bool Test(t,  str ToTest)
{
	try 
	{
		return /amb(_)!:= parse(t, ToTest);
	}
  	catch x: return false;
}   
  
 test bool tF01() =  Test(#start[Program],readFile(|file:///Users/Bouke/Documents/Git/ogol/input/test.ogol|));
 test bool tF02() =  Test(#start[Program],readFile(|file:///Users/Bouke/Documents/Git/ogol/input/trees.ogol|));
 test bool tF03() =  Test(#start[Program],readFile(|file:///Users/Bouke/Documents/Git/ogol/input/octagon.ogol|));
 test bool tF04() =  Test(#start[Program],readFile(|file:///Users/Bouke/Documents/Git/ogol/input/dashed.ogol|));
 
 public void tx1() { renderParsetree(parse(#Program, "home;")); }
 public void tx2() { renderParsetree((Program)`home;`); }
 public void tx3() { renderParsetree((Program)`pendown;`); }
 public void tx4() { renderParsetree(parse(#Expr, "5.0")); }
  