module ogol::Desugar

import ogol::Syntax;

Program desugar(Program p){
	return visit (p) {
 	case (Command)`fd <Expr e>;`
 	  => (Command)`forward<Expr e>;`
 	 case (Command)`bk <Expr e>;`
 	  => (Command)`back<Expr e>;`
 	 case (Command)`rt <Expr e>;`
 	  => (Command)`right<Expr e>;`
 	  case (Command)`lt <Expr e>;`
 	  => (Command)`left<Expr e>;`
 	  case (Command)`pu;`
 	  => (Command)`penup;`
 	  case (Command)`pd;`
 	  => (Command)`pendown;`
 	  default: p; 
	};
}