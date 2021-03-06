module ogol::Eval
import Prelude;
import ogol::Syntax;
import ogol::Canvas;
import ogol::Desugar;
import IO;
import util::Math;

alias FunEnv = map[FunId id, FunDef def];

alias VarEnv = map[VarId id, Value val];

data Value
  = boolean(bool b)
  | number(real i)
  ;

/*
         +y
         |
         |
         |
-x ------+------- +x
         |
         |
         |
        -y

NB: home = (0, 0)
*/



alias Turtle = tuple[int dir, bool pendown, Point position];

alias State = tuple[Turtle turtle, Canvas canvas];

// Top-level eval function
// todo: Can't seem to get this to work
//FunEnv collectFunDefs(Program p)
//  = ( f.id: f | /FunDef f := p );
// but that lead to an error on f.id. It seems like I should somehow name the FunId part of the FunDef "id"
// but I don't know how to do that

// So I tried to use pattern matching like this
//FunEnv collectFunDefs(Program p)
//  = ( fid: f | f:(Program)`to <FunId fid> <VarId* vids> <Command* cmds> end` := p )
// which detect the FunDef ok, but since we match it as a program it complains that I'm returning
// a map[FunId, Program] instead of a map[FunId, FunDef]. 
// This boils down to the same problem I have with desugar: If I match something as a Program, I know
// it is a list of commands, but I can't seem to find out how to work on it like a list of commands.
// Here we have detected a FunDef, but again I don't know how to work with it as a FunDef afterwards
// FunEnv collectFunDefs(Program p)
// = ( fid: f | f:(FunDef)`to <FunId fid> <VarId* vids> <Command* cmds> end` := p )
// will not work either we have alreadt declared p a Program
// Hey, this seems to work!

FunEnv collectFunDefs(Program p)
{
   FunEnv fe =();
   visit (p)	
   {
     case fd:(FunDef)`to <FunId fid> <VarId* vids> <Command* cmds> end`:
     fe[fid] = fd;
   } 
   return fe;     
}

   

Canvas eval(p:(Program)`<Command* cmds>`) {
// todo: How do I desugar here?!
  funenv = collectFunDefs(p);
  varEnv = ();
  state = <<0, false, <0,0>>, []>; 
  
  for (c <- cmds) {
    state = eval(c, funenv, varEnv, state); 
  }

  return state.canvas;
}

State eval(cmd:(Command)`<FunId funcName> <Expr* args>;`, FunEnv fenv, VarEnv venv, State state) 
{
	println(args[0]);
	println(args[1]);
		
	f = fenv[funcName];
	if((FunDef)`to <FunId fid> <VarId* vids> <Command* cmds> end` := f)// && size(vids) == size(args))
	{
		int i = 0;
		println(args[0]);
		println(args[1]);
		//println(vids);
		//for(VarId vid <- vids)
		//{
		//	println(vid);
		//}
		//for(Expr arg <- args)
		//{
		//	println(eval(arg, venv)); 
		//}
		//println(cmd);
	}
	return <<0, false, <0,0>>, []>; 
}

State eval((Command)FunDef, FunEnv fenv, VarEnv venv, State state)
{
	// Should we do anything here? It's only the definition and we already filled the FunEnv...
	return state;
}

State eval((Command)`if <Expr ex> [<Command* cmds>]`, FunEnv fenv, VarEnv venv, State state)
{
	if(boolean(exec) := eval(ex, venv) && exec)
	{
		for (c <- cmds) {
    		state = eval(c, fenv, venv, state);
    } 
  }
	return state;
}

State eval((Command)`ifelse <Expr ex> [<Command* cmdsYes>] [<Command* cmdsNo>]`, FunEnv fenv, VarEnv venv, State state)
{
	if(boolean(exec) := eval(ex, venv))
	{
		if(exec)
		{
			for (c <- cmdsYes) {
    			state = eval(c, fenv, venv, state);
    		}
    	}
    	else
    	{
    		for (c <- cmdsNo) {
    			state = eval(c, fenv, venv, state);
    		}
    	}
    	return state;
    }   
}

State eval((Command)`repeat <Expr ex> [<Command* cmds>]`, FunEnv fenv, VarEnv venv, State state)
{
	if(number(count) := eval(ex, venv))
	{
		for(int i <- [1..round(count)+1]) 
		{
			for (c <- cmds) {
    		state = eval(c, fenv, venv, state);
    		}
		}
		return state;
    }   
}

State eval((Command)`home;`, FunEnv fenv, VarEnv venv, State state) 
{
	if(state.turtle.pendown)
	{
		Shape pos = line(state.turtle.position, <0,0>);
		state.canvas = state.canvas + pos;
	}
	
	state.turtle.position.x = 0;
	state.turtle.position.y = 0;
	
	return state;
}

State eval((Command)`forward <Expr x>;`, FunEnv fenv, VarEnv venv, State state) 
{
	return moveTurtle(x, state.turtle.dir, fenv, venv, state);
}

State eval((Command)`back <Expr x>;`, FunEnv fenv, VarEnv venv, State state) 
{
	return moveTurtle(x, (state.turtle.dir + 180) % 360, fenv, venv, state);
}

State eval((Command)`left <Expr x>;`, FunEnv fenv, VarEnv venv, State state) 
{
	if( number(toLeft) := eval(x, venv))
	{
		state.turtle.dir = (state.turtle.dir - round(toLeft));
		while(state.turtle.dir < 0) state.turtle.dir = state.turtle.dir + 360;
	}
	return state;
}

State eval((Command)`right <Expr x>;`, FunEnv fenv, VarEnv venv, State state) 
{
	if( number(toRight) := eval(x, venv))
	{
		state.turtle.dir = (state.turtle.dir + round(toRight));
		while(state.turtle.dir >= 360) state.turtle.dir = state.turtle.dir - 360;
	}
	return state;
}

State eval((Command)`pendown;`, FunEnv fenv, VarEnv venv, State state) 
{
	state.turtle.pendown = true;
	return state;
}

State eval((Command)`penup;`, FunEnv fenv, VarEnv venv, State state) 
{
	state.turtle.pendown = false;
	return state;
}

State moveTurtle(Expr x, int dir, FunEnv fenv, VarEnv venv, State state)
{
	if( number(moveLength) := eval(x, venv))
	{	
		Point newPos = move(moveLength, dir, state.turtle.position);
		
		if(state.turtle.pendown)
		{
			Shape pos = line(state.turtle.position, newPos);
			state.canvas = state.canvas + pos;
		}
		
		state.turtle.position = newPos;
		
		return state;
	}
}

Point move(real distance, int dir, Point currPos)
{
	angleRad = toReal(dir) / 360.0 * 2.0 * PI();
	moveX = cos(angleRad) * distance;
	moveY = sin(angleRad) * distance;
	int newPosX = round(currPos.x + moveX);
	int newPosY = round(currPos.y + moveY);
	
	return <newPosX, newPosY>;
}


//"forward" Expr ";" | "fd" Expr ";" |
//				 "back"    Expr ";" | "bk" Expr ";" |
//				 "right"   Expr ";" | "rt" Expr ";" |
//				 "left"    Expr ";" | "lt" Expr ";" |
//				 "home" |

//default State eval(Command cmd, FunEnv fenv, VarEnv venv, State state)
//{
//}

// Simple stuff
Value eval(Expr e, VarEnv venv) 
{
	switch(e)
	{
		case (Expr)`true` : return boolean(true);
		case (Expr)`false` : return boolean(false);
		case (Expr)`<Num n>` : return number(toReal(unparse(n)));
		case (Expr)`<VarId x>` : return eval(x, venv);
	}
}


//Value eval((Expr)`true`, VarEnv env) = boolean(true);
//Value eval((Expr)`<Num n>`, VarEnv env)
//	= number(toReal(unparse(n)));
	
// Variables
Value eval(VarId x, VarEnv env)	= env[x];
	
// Arithmetic
Value eval((Expr)`<Expr lhs> * <Expr rhs>`, VarEnv env)
	= number(x * y)
		when 
			number(x) := eval(lhs, env),		// Check dat x en y nummbers zijn
			number(y) := eval(rhs, env);
Value eval((Expr)`<Expr lhs> / <Expr rhs>`, VarEnv env)
	= number(x / y)
		when 
			number(x) := eval(lhs, env),		// Check dat x en y nummbers zijn
			number(y) := eval(rhs, env);
Value eval((Expr)`<Expr lhs> + <Expr rhs>`, VarEnv env)
	= number(x + y)
		when 
			number(x) := eval(lhs, env),		// Check dat x en y nummbers zijn
			number(y) := eval(rhs, env);
Value eval((Expr)`<Expr lhs> - <Expr rhs>`, VarEnv env)
	= number(x - y)
		when 
			number(x) := eval(lhs, env),		// Check dat x en y nummbers zijn
			number(y) := eval(rhs, env);
			
// Compare <, <=, >, >=, =, !=
Value eval((Expr)`<Expr lhs> \< <Expr rhs>`, VarEnv env)
	= boolean(x < y)
		when 
			number(x) := eval(lhs, env),		// Check dat x en y nummbers zijn
			number(y) := eval(rhs, env);
			
Value eval((Expr)`<Expr lhs> \<= <Expr rhs>`, VarEnv env)
	= boolean(x <= y)
		when 
			number(x) := eval(lhs, env),		// Check dat x en y nummbers zijn
			number(y) := eval(rhs, env);
			
Value eval((Expr)`<Expr lhs> \> <Expr rhs>`, VarEnv env)
	= boolean(x > y)
		when 
			number(x) := eval(lhs, env),		// Check dat x en y nummbers zijn
			number(y) := eval(rhs, env);
			
Value eval((Expr)`<Expr lhs> \>= <Expr rhs>`, VarEnv env)
	= boolean(x >= y)
		when 
			number(x) := eval(lhs, env),		// Check dat x en y nummbers zijn
			number(y) := eval(rhs, env);
			
Value eval((Expr)`<Expr lhs> = <Expr rhs>`, VarEnv env)
	= boolean(lhs == rhs);

Value eval((Expr)`<Expr lhs> != <Expr rhs>`, VarEnv env)
	= boolean(lhs != rhs);
	
Value eval((Expr)`<Expr lhs> && <Expr rhs>`, VarEnv env)
	= boolean(x && y)
		when 
			boolean(x) := eval(lhs, env),		// Check dat x en y nummbers zijn
			boolean(y) := eval(rhs, env);
			
Value eval((Expr)`<Expr lhs> || <Expr rhs>`, VarEnv env)
	= boolean(x || y)
		when 
			boolean(x) := eval(lhs, env),		// Check dat x en y nummbers zijn
			boolean(y) := eval(rhs, env);

							
//map[FunId id, FunDef def]		
FunEnv  collectFunDefs(Program p)
	 = ( f.id: f | /FunDef f := p);
			


test bool testTrue() = eval((Expr)`true`, ()) == boolean(true);
test bool testVar() 
	= eval((Expr)`:x`, ((VarId)`:x` : number(1.0))) == number(1.0);
test bool testMul() 
	= eval((Expr)`:x * 3.0`, ((VarId)`:x` : number(2.0))) == number(6.00);	
	
	
test bool testBool01() = eval((Expr)`true`, ()) == boolean(true);
test bool testBool01() = eval((Expr)`false`, ()) == boolean(false);

test bool testNum01() = eval((Expr)`1.0`, ()) == number(1.0);
test bool testNum02() = eval((Expr)`-81.2`, ()) == number(-81.2);

test bool testVar01()
	= eval((Expr)`:x`, ((VarId)`:x` : number(1.0))) == number(1.0);
test bool testVar02()
	= eval((Expr)`:x`, ((VarId)`:x` : boolean(false))) == boolean(false);
	
test bool testMul01() 
	= eval((Expr)`:x * 3.0`, ((VarId)`:x` : number(2.0))) == number(6.00);	

test bool testDiv01() 
	= eval((Expr)`:x / 3.0`, ((VarId)`:x` : number(6.0))) == number(2.);	
	
test bool testMin01() 
	= eval((Expr)`:x + 3.0`, ((VarId)`:x` : number(2.0))) == number(5.0);	
	
test bool testAdd01() 
	= eval((Expr)`:x - 3.0`, ((VarId)`:x` : number(5.0))) == number(2.0);	
	
test bool testLT01()
	= eval((Expr)`4.0 \< 5.0`, ()) == boolean(true);
test bool testLT02()
	= eval((Expr)`4.0 \< 4.0`, ()) == boolean(false);
test bool testLT03()
	= eval((Expr)`4.0 \< 3.0`, ()) == boolean(false);
	
test bool testLE01()
	= eval((Expr)`4.0 \<= 5.0`, ()) == boolean(true);
test bool testLE02()
	= eval((Expr)`4.0 \<= 4.0`, ()) == boolean(true);
test bool testLE03()
	= eval((Expr)`4.0 \<= 3.0`, ()) == boolean(false);
	
test bool testGT01()
	= eval((Expr)`4.0 \> 5.0`, ()) == boolean(false);
test bool testGT02()
	= eval((Expr)`4.0 \> 4.0`, ()) == boolean(false);
test bool testGT03()
	= eval((Expr)`4.0 \> 3.0`, ()) == boolean(true);
	
test bool testGE01()
	= eval((Expr)`4.0 \>= 5.0`, ()) == boolean(false);
test bool testGE02()
	= eval((Expr)`4.0 \>= 4.0`, ()) == boolean(true);
test bool testGE03()
	= eval((Expr)`4.0 \>= 3.0`, ()) == boolean(true);
	
test bool testEq01() = 
	eval((Expr)`4.0 = 4.0`, ()) == boolean(true);
test bool testEq02() = 
	eval((Expr)`4.1 = 4.0`, ()) == boolean(false);
	
test bool testEq01() = 
	eval((Expr)`4.0 != 4.0`, ()) == boolean(false);
test bool testEq02() = 
	eval((Expr)`4.1 != 4.0`, ()) == boolean(true);
	
test bool testAnd01() = 
	eval((Expr)`true && false`, ()) == boolean(false);
test bool testAnd02() = 
	eval((Expr)`false && false`, ()) == boolean(false);
test bool testAnd03() = 
	eval((Expr)`true && true`, ()) == boolean(true);
	
test bool testOr01() = 
	eval((Expr)`true || false`, ()) == boolean(true);
test bool testOr02() = 
	eval((Expr)`false || false`, ()) == boolean(false);
test bool testOr03() = 
	eval((Expr)`true || true`, ()) == boolean(true);