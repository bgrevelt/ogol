module ogol::CallGraph

import Prelude;
import Set;
import Relation;
import ogol::Syntax;
import analysis::graphs::Graph;


alias callGraph = Graph[str];

alias FunctionDefinitions = lrel[str funName, str scopeName];
alias RetVal = tuple[callGraph cg, FunctionDefinitions fdefs];

callGraph getCg(p:(Program)`<Command* cmds>`) 
{
  return getCg("global", cmds, []).cg;
}

RetVal getCg(str scopeName, Command* commands, FunctionDefinitions fdefs)
{ 
	callGraph cg = {};
	FunctionDefinitions lfdefs = fdefs;
	for(cmd <- commands)
	{
		println("<cmd>");
		RetVal ret = getCg(scopeName, cmd, fdefs);
		cg += ret.cg;
		lfdefs = ret.fdefs + lfdefs;
		println(lfdefs);
		//getCg(scopeName, cmd, fdefs);
	}
	return <cg, lfdefs>;
	//return {*getCg(scopeName, cmd, fdefs) | cmd <- commands};
}
 
 
RetVal getCg(str scopeName, (Command)`to <FunId fid> <VarId* vids> <Command* cmds> end`, FunctionDefinitions fdefs)
{
	str newScope;
	if(scopeName != "global") newScope = "<scopeName>/<fid>";
	else newScope = "<fid>";
	
	fdefs = <"<fid>", scopeName> + fdefs;
	//println(fdefs);
	return getCg(newScope, cmds, fdefs);
}

RetVal getCg(str scopeName, (Command)`<FunId fid> <Expr* xs>;`, FunctionDefinitions fdefs)
{
	callGraph cg = {};
	//println(fdefs);	println(scopeName); println("<fid>"); println("");
	if(scopeName == "global") return <cg, fdefs>;
	
	list[str] funDefs = fdefs["<fid>"];
    if(size(funDefs) > 0)
    {
    	return <{ <scopeName, "<funDefs[0]>/<fid>"> },fdefs>;
    }
	else
	{
		//println(fdefs);
		return <{<scopeName, "unknown/<fid>"> }, fdefs>;
	}
}

RetVal getCg(str scopeName, (Command)`if <Expr ex> [<Command* cmds>]`, FunctionDefinitions fdefs)
{
	return getCg(scopeName, cmds, fdefs);
}

RetVal getCg(str scopeName, (Command)`ifelse <Expr ex> [<Command* cmdsYes>] [<Command* cmdsNo>]`, FunctionDefinitions fdefs)
{
	return getCg(scopeName, cmdsYes, fdefs) + getCg(scopeName, cmdsNo, fdefs);
}

RetVal getCg(str scopeName, (Command)`repeat <Expr ex> [<Command* cmds>]`, FunctionDefinitions fdefs)
{
	return getCg(scopeName, cmds, fdefs);
}

RetVal getCg(str scopeName, Command cmd, FunctionDefinitions fdefs)
{
	return <{},fdefs>;
}


