package AST
import math.pow

object Priority {
    val binary = Map("lambda"->1,
                     "or"->2,
                     "and"->3,
                     "is"->8, "<"->8, ">"->8, ">="->8, "<="->8, "=="->8, "!="->8,
                     "+"->9,  "-"->9,
                     "*"->10, "/"->10, "%"->10,
                     "**"->12)

    val unary = Map("not"->4, "+"->12, "-"->12)
}

// sealed trait Node would be also OK
sealed abstract class Node {
    def toStr = "error: toStr not implemented"
    val indent = " " * 4
}

case class IntNum(value: Integer) extends Node {
    override def toStr = value.toString
    
    def +(that:IntNum) = IntNum(this.value + that.value)
    def -(that:IntNum) = IntNum(this.value - that.value)
    def *(that:IntNum) = IntNum(this.value * that.value)
    def /(that:IntNum) = IntNum(this.value / that.value)
    def %(that:IntNum) = IntNum(this.value % that.value)
    def **(that:IntNum) = IntNum(pow(this.value.toDouble, that.value.toDouble).toInt)
    
    def ==(that:IntNum) = if (this.value == that.value) TrueConst() else FalseConst()
    def !=(that:IntNum) = if (this.value != that.value) TrueConst() else FalseConst()
    def >=(that:IntNum) = if (this.value >= that.value) TrueConst() else FalseConst()
    def <=(that:IntNum) = if (this.value <= that.value) TrueConst() else FalseConst()
    def >(that:IntNum) = if (this.value > that.value) TrueConst() else FalseConst()
    def <(that:IntNum) = if (this.value < that.value) TrueConst() else FalseConst()
}

case class FloatNum(value: Double) extends Node {
    override def toStr = value.toString
}

case class StringConst(value: String) extends Node {
    override def toStr = value
}

case class TrueConst() extends Node {
    override def toStr = "True"
}

case class FalseConst() extends Node {
    override def toStr = "False"
}

case class Variable(name: String) extends Node {
    override def toStr = name
}

case class Unary(op: String, expr: Node) extends Node {

    override def toStr = {
        var str  = expr.toStr 
        expr match {
            case e@BinExpr(_,_,_) => if(Priority.binary(e.op)<=Priority.unary(op)) { str = "(" + str + ")" }
            case e@Unary(_,_) => if(Priority.unary(e.op)<=Priority.unary(op)) { str = "(" + str + ")" }
            case _ => 
        }
        op + " " + str
    }

}

case class BinExpr(op: String, left: Node, right: Node) extends Node {

    override def toStr = {
        var leftStr  = left.toStr 
        var rightStr = right.toStr
        left match {
            case l@(_:BinExpr) => if(Priority.binary(l.op)<Priority.binary(op)) { leftStr = "(" + leftStr + ")" }
            case l@(_:Unary) => if(Priority.unary(l.op)<Priority.binary(op)) { leftStr = "(" + leftStr + ")" }
            case _ => 
        }
        right match {
            case r@BinExpr(_,_,_) => if(Priority.binary(r.op)<Priority.binary(op)) { rightStr = "(" + rightStr + ")" }
            case r@Unary(_,_) => if(Priority.unary(r.op)<Priority.binary(op)) { rightStr = "(" + rightStr + ")" }
            case _ => 
        }
        leftStr + " " + op + " " + rightStr
    }
    
    override def equals(that: Any) = that match {
		//Pierwsze 4 sa po to, ze sa przemienne (tak dodawanie jest przemienne ;D)
		//Przedostatni case pokrywa operatory.
		case BinExpr("+", l, r)   if l == right && r == left  && op == "+"   => true
        case BinExpr("*", l, r)   if l == right && r == left  && op == "*"   => true
        case BinExpr("and", l, r) if l == right && r == left  && op == "and" => true
        case BinExpr("or", l, r)  if l == right && r == left  && op == "or"  => true
        case BinExpr(o, l, r)     if l == left  && r == right && o  == op    => true
        case _                                                               => false
	}
}

case class IfElseExpr(cond: Node, left: Node, right: Node) extends Node {
    override def toStr = left.toStr + " if " + cond.toStr + " else " + right.toStr
}

case class Assignment(left: Node, right: Node) extends Node {
    override def toStr = left.toStr + " = " + right.toStr
}

case class Subscription(expr: Node, sub: Node) extends Node {
    override def toStr = expr.toStr + "[" + sub.toStr + "]"
}

case class KeyDatum(key: Node, value: Node) extends Node {
    override def toStr = key.toStr + ": " + value.toStr
}

case class GetAttr(expr:Node, attr: String) extends Node {
    override def toStr = expr.toStr + "." + attr
}

case class IfInstr(cond: Node, left: Node) extends Node {
    override def toStr = {
        var str = "if " + cond.toStr + ":\n"
        str += left.toStr.replaceAll("(?m)^", indent)
        str
    }
}

case class IfElseInstr(cond: Node, left: Node, right: Node) extends Node {
    override def toStr = {
        var str = "if " + cond.toStr + ":\n"
        str += left.toStr.replaceAll("(?m)^", indent)
        str += "\nelse:\n"
        str += right.toStr.replaceAll("(?m)^", indent)
        str
    }
}

case class WhileInstr(cond: Node, body: Node) extends Node {
    override def toStr = {
        "while " + cond.toStr + ":\n" + body.toStr.replaceAll("(?m)^", indent)
    }
}

case class InputInstr() extends Node {
    override def toStr = "input()" 
}

case class ReturnInstr(expr: Node) extends Node {
    override def toStr = "return " + expr.toStr
}

case class PrintInstr(expr: Node) extends Node {
    override def toStr = "print " + expr.toStr
}

case class FunCall(name: Node, args_list: Node) extends Node {

    override def toStr = {
        args_list match {
            case NodeList(list) => name.toStr + "(" + list.map(_.toStr).mkString("", ",", "") + ")"
            case _ => name.toStr + "(" + args_list.toStr + ")"
        }
    }
}

case class FunDef(name: String, formal_args: Node, body: Node) extends Node {
    override def toStr = {
        var str = "\ndef " + name + "(" + formal_args.toStr + "):\n"
        str += body.toStr.replaceAll("(?m)^", indent) + "\n"
        str
    }
}

case class LambdaDef(formal_args: Node, body: Node) extends Node {
    override def toStr = "lambda " + formal_args.toStr + ": " + body.toStr
}
        
case class ClassDef(name: String, inherit_list: Node, suite: Node) extends Node {
    override def toStr = {
        val str = "\nclass " + name
        var inheritStr = ""
        val suiteStr = ":\n" + suite.toStr.replaceAll("(?m)^", indent)
        inherit_list match {
            case NodeList(x) => if(x.length>0) inheritStr = "(" + x.map(_.toStr).mkString("", ",", "") + ")" 
            case _ =>
       }
       str + inheritStr + suiteStr
    }
}

case class NodeList(list: List[Node]) extends Node {
    override def toStr = {
        list.map(_.toStr).mkString("", "\n", "")
    }
}

case class KeyDatumList(list: List[KeyDatum]) extends Node {
    override def toStr = list.map(_.toStr).mkString("{", ",", "}")
}

case class IdList(list: List[Variable]) extends Node {
    override def toStr = list.map(_.toStr).mkString("", ",", "")
}

case class ElemList(list: List[Node]) extends Node {
    override def toStr = list.map(_.toStr).mkString("[", ",", "]")
}

case class Tuple(list: List[Node]) extends Node {
    override def toStr = if(list.length==0) "()"
                         else if(list.length==1) "(" + list(0).toStr + ",)"
                         else list.map(_.toStr).mkString("(", ",", ")")
}

case class Atrapa() extends Node {
	override def toStr = ""
}
        
