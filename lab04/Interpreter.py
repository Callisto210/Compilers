import AST
import SymbolTable
from Memory import *
from Exceptions import  *
from visit import *
import sys

sys.setrecursionlimit(10000)

class Interpreter(object):
    def __init__(self):
        self.stack = MemoryStack()

    @on('node')
    def visit(self, node):
        pass

    @when(AST.BinExpr)
    def visit(self, node):
        r1 = node.left.accept(self)
        r2 = node.right.accept(self)
        return {
            '+'     : lambda x, y: x + y,
            '-'     : lambda x, y: x - y,
            '*'     : lambda x, y: x * y,
            '/'     : lambda x, y: x / y,
            '%'     : lambda x, y: x % y,
            '|'     : lambda x, y: x | y,
            '&'     : lambda x, y: x & y,
            '^'     : lambda x, y: x ^ y,
            '&&'    : lambda x, y: x and y,
            '||'    : lambda x, y: x or y,
            '<<'    : lambda x, y: x << y,
            '>>'    : lambda x, y: x >> y,
            '=='    : lambda x, y: x == y,
            '!='    : lambda x, y: x != y,
            '>'     : lambda x, y: x > y,
            '<'     : lambda x, y: x < y,
            '<='    : lambda x, y: x <= y,
            '>='    : lambda x, y: x >= y,
        }[node.op](r1, r2)

    @when(AST.Const)
    def visit(self, node):
        print "To sie nie powinno wypisac - NIGDY!"
        return node.value

    @when(AST.Integer)
    def visit(self, node):
        return int(node.value)

    @when(AST.Float)
    def visit(self, node):
        return float(node.value)

    @when(AST.String)
    def visit(self, node):
        return str(node.value)

    @when(AST.Variable)
    def visit(self, node):
        return self.stack.get(node.name)
        
    @when(AST.InstList)
    def visit(self, node):
        for i in node.children:
            i.accept(self)

    @when(AST.ChoiInst)
    def visit(self, node):
        if node.cond.accept(self):
            return node.inst.accept(self)
        elif node.alt:
            return node.alt.accept(self)
        else:
            pass


    @when(AST.WhiInst)
    def visit(self, node):
        while node.cond.accept(self):
            try:
                node.inst.accept(self)
            except BreakException:
                break
            except ContinueException:
                pass

    @when(AST.RepInst)
    def visit(self, node):
        while True:
            try:
                node.inst.accept(self)
                if node.cond.accept(self):
                    return
            except BreakException:
                break
            except ContinueException:
                if node.cond.accept(self):
                    return

    @when(AST.LabInst)
    def visit(self, node):
        pass
        #why??????????????????????

    @when(AST.PrtInst)
    def visit(self, node):
        toPrintList = node.data.accept(self)
        toPrint = ""
        for i in toPrintList:
            toPrint += str(i)
        print toPrint

    @when(AST.Program)
    def visit(self, node):
        node.program.accept(self)
        
    @when(AST.Podprogramy)
    def visit(self, node):
        for i in node.children:
            i.accept(self)
            
    @when(AST.ExpList)
    def visit(self, node):
        expArray = []
        for i in node.children:
            expArray.append(i.accept(self))
        return expArray

    @when(AST.Podprogram)
    def visit(self, node):
        node.podprogram.accept(self)


    @when(AST.CompInst)
    def visit(self, node, calledBy):
        if calledBy is None:
            self.stack.push(Memory("Compound"))
        try :
            node.units.accept(self)
        except (ReturnValueException, BreakException, ContinueException) as E:
            raise E
        finally:
            if calledBy is None:
                self.stack.pop()
            
    @when(AST.Units)
    def visit(self, node):
        for i in node.children:
            i.accept(self)
            
    @when(AST.Unit)
    def visit(self, node):
        node.unit.accept(self)

    @when(AST.RetInst)
    def visit(self, node):
        value = node.exp.accept(self)
        toThrowUp = ReturnValueException(value)
        raise toThrowUp
        
    @when(AST.Break)
    def visit(self, node):
        raise BreakException
        
    @when(AST.Continue)
    def visit(self, node):
        raise ContinueException

    @when(AST.AsgInst)# same as init put -> set
    def visit(self, node):
        value = node.exp.accept(self)
        self.stack.set(node.id, value)
        return value

    @when(AST.Init)
    def visit(self, node):
        value = node.exp.accept(self)
        self.stack.insert(node.id, value)
        return value
        
    @when(AST.InitList)
    def visit(self, node):
        for i in node.children:
            i.accept(self)
        

    @when(AST.Declaration)
    def visit(self, node):
        node.inits.accept(self)

    @when(AST.ArgList) ##  nie jest to zbedne jak w przypadku type checkera?
    def visit(self, node):
        for i in node.children:
            i.accept(self)

    @when(AST.Parameter)
    def visit(self, node):
        return node.name.accept(self) ## jest name, ale tam jest jeszce typ, nie powinno byc typ?

    @when(AST.Function)
    def visit(self, node):
        self.stack.insert(node.name, node)

    @when(AST.CallExp)
    def visit(self, node):
        fun = self.stack.get(node.name)
        mem = Memory("function")
        if fun.parameters is not None: #Jak mamy parametry
            for i in range(0, len(fun.parameters.children)):
                mem.put(fun.parameters.children[i].name, node.args.children[i].accept(self))
                #               argList                         exprList
                #print str(fun.parameters.children[i].name) + " = " + str(node.args.children[i].accept(self))
        self.stack.push(mem)
        try:
            fun.body.accept(self, "ala")
        except ReturnValueException as E:
            return E.value
        finally:
            self.stack.pop()
