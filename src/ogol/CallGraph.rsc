module ogol::CallGraph

import Prelude;
import Set;
import Relation;
import ogol::Syntax;
import analysis::graphs::Graph;


alias callGraph = Graph[str];

callGraph getCg(p:(Program)`<Command* cmds>`) 
{
  return getCg("global", cmds);
}

callGraph getCg(str scopeName, Command* commands) =
 { *getCg(scopeName, cmd) | cmd <- commands };
 
 
callGraph getCg(str scopeName, (Command)`to <FunId fid> <VarId* vids> <Command* cmds> end`)
{
	return getCg("<fid>", cmds);
}

callGraph getCg(str scopeName, (Command)`<FunId fid> <Expr* xs>;`)
{
	callGraph cg = {};
	if(scopeName == "global") return cg;
	
	return { <scopeName, "<fid>"> };
}

callGraph getCg(str scopeName, (Command)`if <Expr ex> [<Command* cmds>]`)
{
	return getCg(scopeName, cmds);
}

callGraph getCg(str scopeName, (Command)`ifelse <Expr ex> [<Command* cmdsYes>] [<Command* cmdsNo>]`)
{
	return getCg(scopeName, cmdsYes) + getCg(scopeName, cmdsNo);
}

callGraph getCg(str scopeName, (Command)`repeat <Expr ex> [<Command* cmds>]`)
{
	return getCg(scopeName, cmds);
}

default callGraph getCg(str scopeName, Command cmd)
{
	return {};
}


