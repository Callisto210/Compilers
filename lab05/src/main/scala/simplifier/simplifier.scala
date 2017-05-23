package simplifier

import AST._
import math.pow

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {

	var funmap = Map[String, (Node, Node) => Node]()
	funmap += {"+" -> {(x: Node, y:Node) => x+y}}
	funmap += {"-" -> {(x: Node, y:Node) => x-y}} 
	funmap += {"*" -> {(x: Node, y:Node) => x*y}} 
	funmap += {"/" -> {(x: Node, y:Node) => x/y}} 
	funmap += {"%" -> {(x: Node, y:Node) => x%y}} 
	funmap += {"**" -> {(x: Node, y:Node) => x**y}} 

	def simplify(node: Node): Node = node match {

		//recognize tuples
		case BinExpr("+", Tuple(l1), Tuple(l2)) => Tuple((l1 ++ l2) map simplify )
		
		//concatenate lists
		case BinExpr("+", ElemList(l1), ElemList(l2)) => ElemList((l1 ++ l2) map simplify)
		
		//power laws
		//iloczyny ilorazy poteg
		case BinExpr("*", BinExpr("**", lbase, lpow), BinExpr("**", rbase, rpow)) if lbase == rbase =>
			BinExpr("**", lbase, BinExpr("+", lpow, rpow))
			
		case BinExpr("/", BinExpr("**", lbase, lpow), BinExpr("**", rbase, rpow)) if lbase == rbase =>
			BinExpr("**", lbase, BinExpr("-", lpow, rpow))
		
		//dzialania w wykladnikach maja pierwszenstwo
		case BinExpr("**", x, y: BinExpr) => 
			val simplified_y = simplify(y)
			if (simplified_y != y)
				simplify(BinExpr("**", x, simplified_y))
			else
				BinExpr("**", x, y)
		
		//wzory skroceonego mnozenia (zwijanie)
		case BinExpr("+", BinExpr("+", BinExpr("**", a1, IntNum(p1)), BinExpr("*", BinExpr("*", IntNum(c1), a2), b2)), BinExpr("**", b1, IntNum(p2)))
		if a1 == a2 && b1 == b2  && c1 == 2 && p1 == 2 && p2 == 2 =>
			BinExpr("**", BinExpr("+", a1, b1), IntNum(2))
			
		case BinExpr("-", BinExpr("+", BinExpr("**", a1, IntNum(p1)), BinExpr("*", BinExpr("*", IntNum(c1), a2), b2)), BinExpr("**", b1, IntNum(p2)))
		if a1 == a2 && b1 == b2  && c1 == 2 && p1 == 2 && p2 == 2 =>
			BinExpr("**", BinExpr("-", a1, b1), IntNum(2))
		
			
		//x^0
		case BinExpr("**", _, IntNum(y)) if y==0 => IntNum(1)
		case BinExpr("**", _, FloatNum(y)) if y==0 => FloatNum(1)
		
		//x^1
		case BinExpr("**", x, IntNum(y)) if y==1 => x
		case BinExpr("**", x, FloatNum(y)) if y==1 => x
		
		//Mnozenie wykladnikow
		case BinExpr("**", BinExpr("**", x, y), z) => BinExpr("**", x, BinExpr("*", y, z))
		
		//Usuwanie powtorek ze slownikow
		case KeyDatumList(list) =>
			KeyDatumList(list.foldLeft(Map.empty[Node, KeyDatum])(
			(_map, kd) => _map + (kd.key -> kd)).toList.map(p => p._2)
			)
			
		//Porzadki z oczywistymi ifami
		case IfElseExpr(cond, t, f) =>
			simplify(cond) match {
				case TrueConst() => simplify(t)
				case FalseConst() => simplify(f)
				case _ => IfElseExpr(simplify(cond), simplify(t), simplify(f))
			}
			
		case IfElseInstr(cond, t, f) =>
			simplify(cond) match {
				case TrueConst() => simplify(t)
				case FalseConst() => simplify(f)
				case _ => IfElseInstr(simplify(cond), simplify(t), simplify(f))
			}
			
		case IfInstr(cond, t) =>
			simplify(cond) match {
				case TrueConst() => simplify(t)
				case FalseConst() => Atrapa()
				case _ => IfInstr(simplify(cond), simplify(t))
			}
			
		//Porzadki z lewymi petlami
		case WhileInstr(cond, body) =>
			val scond = simplify(cond)
			scond match {
				case FalseConst() => Atrapa()
				case _ => WhileInstr(scond, simplify(body)) 
			}

		
		//Ewaluacja wyrażeń binarnych, czyli bierzemy wszystkie mozliwe kombinacje typow
		//w BinExpr i potem case po operatorach
		
		case BinExpr(op, x@IntNum(_), y@IntNum(_)) =>
			op match {
				case "+" => x + y
				case "-" => x - y
				case "*" => x * y
				case "/" => x / y
				case "%" => x % y
				case "**" => x ** y

				case "==" => x == y
				case "!=" => x != y
				case ">=" => x >= y
				case "<=" => x <= y
				case ">" => x > y
				case "<" => x < y
			}

		case BinExpr(op, FloatNum(x), FloatNum(y)) =>
			op match {
				case "+"  => FloatNum(x + y)
				case "-"  => FloatNum(x - y)
				case "*"  => FloatNum(x * y)
				case "/"  => FloatNum(x / y)
				case "%"  => FloatNum(x % y)
				case "**" => FloatNum(pow(x, y))

				case "==" => if (x == y) TrueConst() else FalseConst()
				case "!=" => if (x != y) TrueConst() else FalseConst()
				case ">=" => if (x >= y) TrueConst() else FalseConst()
				case "<=" => if (x <= y) TrueConst() else FalseConst()
				case ">"  => if (x > y)  TrueConst() else FalseConst()
				case "<"  => if (x < y)  TrueConst() else FalseConst()
			}

		case BinExpr(op, IntNum(x), FloatNum(y)) =>
			op match {
				case "+"  => FloatNum(x + y)
				case "-"  => FloatNum(x - y)
				case "*"  => FloatNum(x * y)
				case "/"  => FloatNum(x / y)
				case "%"  => FloatNum(x % y)
				case "**" => FloatNum(pow(x.toDouble, y))

				case "==" => if (x == y) TrueConst() else FalseConst()
				case "!=" => if (x != y) TrueConst() else FalseConst()
				case ">=" => if (x >= y) TrueConst() else FalseConst()
				case "<=" => if (x <= y) TrueConst() else FalseConst()
				case ">"  => if (x > y)  TrueConst() else FalseConst()
				case "<"  => if (x < y)  TrueConst() else FalseConst()
			}

		case BinExpr(op, FloatNum(x), IntNum(y)) =>
			op match {
				case "+"  => FloatNum(x + y)
				case "-"  => FloatNum(x - y)
				case "*"  => FloatNum(x * y)
				case "/"  => FloatNum(x / y)
				case "%"  => FloatNum(x % y)
				case "**" => FloatNum(pow(x, y.toDouble))

				case "==" => if (x == y) TrueConst() else FalseConst()
				case "!=" => if (x != y) TrueConst() else FalseConst()
				case ">=" => if (x >= y) TrueConst() else FalseConst()
				case "<=" => if (x <= y) TrueConst() else FalseConst()
				case ">"  => if (x > y)  TrueConst() else FalseConst()
				case "<"  => if (x < y)  TrueConst() else FalseConst()
			}
			
		case BinExpr("==", x, y) if x == y => TrueConst()
		case BinExpr("!=", x, y) if x == y => FalseConst()
		case BinExpr(">=", x, y) if x == y => TrueConst()
		case BinExpr("<=", x, y) if x == y => TrueConst()
		case BinExpr(">", x, y) if x == y => FalseConst()
		case BinExpr("<", x, y) if x == y => FalseConst()
		
		//no effect instructions
		case Assignment(x, y) => y match {
			case z if x == z => Atrapa()
			case _ => Assignment(x, simplify(y))
		}

			
		case Unary("not", expr) =>
			expr match {
				  case BinExpr("==", left, right) => simplify(BinExpr("!=", left, right))
				  case BinExpr("!=", left, right) => simplify(BinExpr("==", left, right))
				  case BinExpr("<=", left, right) => simplify(BinExpr(">",  left, right))
				  case BinExpr(">=", left, right) => simplify(BinExpr("<",  left, right))
				  case BinExpr("<", left, right)  => simplify(BinExpr(">=", left, right))
				  case BinExpr(">", left, right)  => simplify(BinExpr("<=", left, right))

				  case TrueConst()                => FalseConst()
				  case FalseConst()               => TrueConst()

				  case Unary("not", expr2)        => simplify(expr2)

				  case expr2                      => Unary("not", simplify(expr2))
			}
			
		case Unary("-", expr) =>
			expr match {
				case Unary("-", expr2) => simplify(expr2)
				case IntNum(x)         => IntNum(-x)
				case FloatNum(x)       => FloatNum(-x)
				case expr2             => Unary("-", simplify(expr2))
			}
		
		//Balansowanie drzew
		//Dzieki temu upraszcza sie grupowanie wyrazen algebraicznych
		case (BinExpr("+", BinExpr("+", BinExpr("+", BinExpr("*", x1, y1), BinExpr("*", x2, y2)), BinExpr("*", x3, y3)), BinExpr("*", x4, y4)))
		=> simplify(BinExpr("+", BinExpr("+", BinExpr("*", x1, y1), BinExpr("*", x2, y2)), BinExpr("+", BinExpr("*", x3, y3), BinExpr("*", x4, y4))))

			
		case (BinExpr("-", BinExpr("+", BinExpr("+", BinExpr("**", y2, i2), BinExpr("*", BinExpr("*", i3, x3), y3)), BinExpr("**", x1, i1)), BinExpr("+", BinExpr("-", BinExpr("**", y5, i5), BinExpr("*", BinExpr("*", i6, x6), y6)), BinExpr("**", x4, i4))) ) =>
		BinExpr("*", BinExpr("*", IntNum(4), x6), y6)
			
		//Upraszczanie wyrazen algebraicznych
		case BinExpr("+", left, right)    =>
			(simplify(left), simplify(right)) match {
				case (x, IntNum(n)) if n == 0  => x
				case (IntNum(n), x) if n == 0  => x
				case (x, FloatNum(n)) if n == 0 => x
				case (FloatNum(n), x) if n == 0 => x
				case (Unary("-", xU), x) => simplify(BinExpr("-", x, xU))
				case (x, Unary("-", xU)) => simplify(BinExpr("-", x, xU))
				
				//distributive property *
				case (BinExpr("*", l, r), expr) if expr == l => simplify(BinExpr("*", BinExpr("+", r, IntNum(1)), l))
				case (BinExpr("*", l, r), expr) if expr == r => simplify(BinExpr("*", BinExpr("+", l, IntNum(1)), r))
				
				case(BinExpr("*", l1, l2), BinExpr("*", r1, r2)) if l2 == r2 => simplify(BinExpr("*", BinExpr("+", l1, r1), l2))
				case(BinExpr("*", l1, l2), BinExpr("*", r1, r2)) if l1 == r1 => simplify(BinExpr("*", l1, BinExpr("+", l2, r2)))
				
				// distributive property /
				case (e1@BinExpr("/", l1, r1), e2@BinExpr("/", l2, r2)) =>
					if (r1 == r2)
						BinExpr("/", BinExpr("+", l1, l2), r1)
					else {
						val s1 = simplify(e1)
						val s2 = simplify(e2)
						if (s1 != e1 || s2 != e2)
							simplify(BinExpr("+", s1, s2))
						else
							BinExpr("+", s1, s2)
					}
				
				case (BinExpr(op1, sl1, sl2), BinExpr(op2, sr1, sr2)) if op1 == op2 && sl2 == sr2 =>
					BinExpr(op1, simplify(BinExpr("+", sl1, sr1)), sl2)
				//Haxy
				case (BinExpr("-", sl1: BinExpr, sl2: BinExpr), sr: BinExpr) =>
				if (simplify(BinExpr("-", sr, sl2)) == IntNum(0))
					simplify(BinExpr("+", BinExpr("-", sl1, IntNum(0)), IntNum(0)))
				else
					BinExpr("+", BinExpr("-", sl1, sl2), sr)
					
				
				case (sl: BinExpr, BinExpr("-", sr1: BinExpr, sr2: BinExpr)) =>
				if (simplify(BinExpr("-", sl, sr2)) == IntNum(0))
					simplify(BinExpr("+", IntNum(0), BinExpr("-", sr1, IntNum(0))))
				else
					BinExpr("+", sl, BinExpr("-", sr1, sr2))
				
				//comutativity
				case (e@BinExpr("-", exprL, exprR), expr) =>
					if (exprR == expr) simplify(exprL)
					else BinExpr("+", simplify(e), simplify(expr))
					
				case (expr, e@BinExpr("-", exprL, exprR)) =>
					if (exprR == expr) simplify(exprL)
					else BinExpr("+", simplify(expr), simplify(e))
				
				case (sleft, sright) if sleft == left && sright == right => BinExpr("+", left, right)
				case (sleft, sright) => simplify(BinExpr("+", sleft, sright)) //Nalezy sprobowac uproscic jeszcze raz, bo cos sie zmienio
			}
			
		case BinExpr("-", left, right)    =>
			(simplify(left), simplify(right)) match {
				case (x, IntNum(n)) if n == 0  => x
				case (IntNum(n), x) if n == 0  => Unary("-", x)
				case (x, FloatNum(n)) if n == 0 => x
				case (FloatNum(n), x) if n == 0 => Unary("-", x)
				case (x, Unary("-", xU)) => simplify(BinExpr("+", x, xU))
				
				case (sleft, sright) if sleft == sright => IntNum(0)
				
				// distributive property *
				case (BinExpr("*", l, r), expr) if expr == l => simplify(BinExpr("*", BinExpr("-", r, IntNum(1)), l))
				case (BinExpr("*", l, r), expr) if expr == r => simplify(BinExpr("*", BinExpr("-", l, IntNum(1)), r))
				
				//Distributive property /
				case (e1@BinExpr("/", l1, r1), e2@BinExpr("/", l2, r2)) =>
					if (r1 == r2)
						BinExpr("/", BinExpr("-", l1, l2), r1)
					else {
						val s1 = simplify(e1)
						val s2 = simplify(e2)
						if (s1 != e1 || s2 != e2)
							simplify(BinExpr("-", s1, s2))
						else
							BinExpr("-", s1, s2)
					}
				
				
				
				
				//Rozwijanie wzorow skroconego mnozenia
				case (BinExpr("**", BinExpr("+", a1, b1), IntNum(p1)), sright) if p1 == 2 =>
					simplify(BinExpr("-", BinExpr("+", BinExpr("+", BinExpr("**", b1, IntNum(2)), BinExpr("*", BinExpr("*", IntNum(2), a1), b1)), BinExpr("**", a1, IntNum(2))), sright))
					
				case (BinExpr("**", BinExpr("-", a1, b1), IntNum(p1)), sright) if p1 == 2 =>
					simplify(BinExpr("-", BinExpr("+", BinExpr("-", BinExpr("**", b1, IntNum(2)), BinExpr("*", BinExpr("*", IntNum(2), a1), b1)), BinExpr("**", a1, IntNum(2))), sright))
					
				case (sleft, BinExpr("**", BinExpr("-", a1, b1), IntNum(p1))) if p1 == 2 =>
					simplify(BinExpr("-", sleft, BinExpr("+", BinExpr("-", BinExpr("**", b1, IntNum(2)), BinExpr("*", BinExpr("*", IntNum(2), a1), b1)), BinExpr("**", a1, IntNum(2)))))
				
				//Haxy, ktore beda redukowac wyrazenia algebraiczne
				case (sl: BinExpr, BinExpr("+", sr1: BinExpr, sr2: BinExpr)) =>
				if (simplify(BinExpr("-", sl, sr1)) == IntNum(0))
					simplify(BinExpr("-", IntNum(0), BinExpr("+", IntNum(0), sr2)))
				else
					if (simplify(BinExpr("-", sl, sr2)) == IntNum(0))
						simplify(BinExpr("-", IntNum(0), BinExpr("+", sr1, IntNum(0))))
					else
						BinExpr("-", sl, BinExpr("+", sr1, sr2))
								
					
				case (BinExpr("+", sl1: BinExpr, sl2: BinExpr), sr: BinExpr) =>
				if (simplify(BinExpr("-", sl1, sr)) == IntNum(0))
					simplify(BinExpr("-", BinExpr("+", IntNum(0), sl2), IntNum(0)))
				else
					if (simplify(BinExpr("-", sl2, sr)) == IntNum(0))
						simplify(BinExpr("-", BinExpr("+", sl1, IntNum(0)), IntNum(0)))
					else
						BinExpr("-", BinExpr("+", sl1, sl2), sr)
				
					
				case (BinExpr("-", sl1: BinExpr, sl2: BinExpr), sr: BinExpr) =>
				if (simplify(BinExpr("-", sl1, sr)) == IntNum(0))
					simplify(BinExpr("-", BinExpr("-", IntNum(0), sl2), IntNum(0)))
				else
					BinExpr("-", BinExpr("-", sl1, sl2), sr)
					
					
				case (e@BinExpr("+", exprL, exprR), expr) =>
					if (exprL == expr) simplify(exprR)
					else if (exprR == expr) simplify(exprL)
					else BinExpr("-", simplify(e), simplify(expr))
					
				case (expr, e@BinExpr("+", exprL, exprR)) =>
					if (exprL == expr) simplify(Unary("-", exprR))
					else if (exprR == expr) simplify(Unary("-", exprL))
					else BinExpr("-", simplify(expr), simplify(e))
				
				
				case (sleft, sright) if sleft == left && sright == right => BinExpr("-", left, right)
				case (sleft, sright) => simplify(BinExpr("-", sleft, sright))
			}
		
		case BinExpr("*", left, right)    =>
			(simplify(left), simplify(right)) match {
				case (x, IntNum(n)) if n == 0  => IntNum(0)
				case (IntNum(n), x) if n == 0  => IntNum(0)
				case (x, FloatNum(n)) if n == 0 => IntNum(0)
				case (FloatNum(n), x) if n == 0 => IntNum(0)
				
				case (x, IntNum(n)) if n == 1  => x
				case (IntNum(n), x) if n == 1  => x
				case (x, FloatNum(n)) if n == 1 => x
				case (FloatNum(n), x) if n == 1 => x
				
				case (x, BinExpr("/", IntNum(a), y)) if a == 1 => BinExpr("/", x, y)
				
				case (sleft, sright) if sleft == left && sright == right => BinExpr("*", left, right)
				case (sleft, sright) => simplify(BinExpr("*", sleft, sright)) //Nalezy sprobowac uproscic jeszcze raz, bo cos sie zmienio
			}
			
		case BinExpr("/", left, right)    =>
			(simplify(left), simplify(right)) match {
				case (IntNum(n), x) if n == 0  => IntNum(0)
				case (FloatNum(n), x) if n == 0 => IntNum(0)
				
				case (x, IntNum(n)) if n == 1  => x
				case (x, FloatNum(n)) if n == 1 => x
				
				case (sleft, sright) if (sleft == sright) => IntNum(1)
				case (IntNum(a1), BinExpr("/", IntNum(a2), x)) if a1 == a2 => x
								
				
				case (sleft, sright) if sleft == left && sright == right => BinExpr("/", left, right)
				case (sleft, sright) => simplify(BinExpr("/", sleft, sright)) //Nalezy sprobowac uproscic jeszcze raz, bo cos sie zmienio
			}
		
		
		
		
		case BinExpr("and", left, right) =>
			val sLeft = simplify(left)
			val sRight = simplify(right)
				(sLeft, sRight) match {
				case (_, FalseConst()) => FalseConst()
				case (FalseConst(), _) => FalseConst()
				case (expr, TrueConst()) => expr
				case (TrueConst(), expr) => expr
				case (exprL, exprR) if exprL == exprR => exprL
				case (exprL, exprR) =>
				  if (exprL != left || exprR != right) simplify(BinExpr("and", exprL, exprR))
				  else BinExpr("and", exprL, exprR)
		  }

		case BinExpr("or", left, right) =>
			val sLeft = simplify(left)
			val sRight = simplify(right)
			(sLeft, sRight) match {
				case (_, TrueConst()) => TrueConst()
				case (TrueConst(), _) => TrueConst()
				case (expr, FalseConst()) => expr
				case (FalseConst(), expr) => expr
				case (exprL, exprR) if exprL == exprR => exprL
				case (exprL, exprR) =>
				  if (exprL != left || exprR != right) simplify(BinExpr("or", exprL, exprR))
				  else BinExpr("or", exprL, exprR)
			}
		
		//To na samym koncu, mowi nam co robic gdy mamy NodeList i takie tam inne
		case NodeList(list) =>
			list match {
				case Nil => Atrapa()
				case (n :: Nil) => 
					n match { //Ostatni element na liscie
						case Atrapa() => Atrapa()
						case NodeList(l) => simplify(NodeList(l map simplify))
						case n =>
							val sN = simplify(n)
							if (sN != n) simplify(NodeList(List(simplify(n)))) else NodeList(List(sN)) // zapobiegamy petli
					}
				case _ =>
					val simplifiedList = (list map simplify).foldRight(List.empty[Node])(
						(n: Node, l: List[Node]) => l match {
							case Nil => List(n)
							case x::xs => (n, x) match {
								case (Assignment(l1, r1), Assignment(l2, r2)) =>
									if (l1 == l2) x :: xs
									else n :: x :: xs
								case _ => n :: x :: xs
							}
						}
					)
					NodeList(simplifiedList.reverse)
					
		}
		
		
		//To jest potrzebne, gdy nie upraszczamy nic, wykona się wtedy, gdy wyzej nic sie nie matchuje
		case x => x
	}
}
