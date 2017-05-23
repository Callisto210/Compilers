from collections import defaultdict
import AST
from SymbolTable import SymbolTable, FunctionSymbol, VariableSymbol

type_dict = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: None)))
for op in ['+', '-', '*', '/', '%', '<', '>', '<<', '>>', '|', '&', '^', '<=', '>=', '==', '!=']:
    type_dict[op]['int']['int'] = 'int'

for op in ['+', '-', '*', '/']:
    type_dict[op]['int']['float'] = 'float'
    type_dict[op]['float']['int'] = 'float'
    type_dict[op]['float']['float'] = 'float'

for op in ['<', '>', '<=', '>=', '==', '!=']:
    type_dict[op]['int']['float'] = 'int'
    type_dict[op]['float']['int'] = 'int'
    type_dict[op]['float']['float'] = 'int'

type_dict['+']['string']['string'] = 'string'
type_dict['*']['string']['int'] = 'string'

for op in ['<', '>', '<=', '>=', '==', '!=']:
    type_dict[op]['string']['string'] = 'int'


class NodeVisitor(object):
    def visit(self, node):
        method = 'visit_' + node.__class__.__name__
        visitor = getattr(self, method, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        if isinstance(node, list):
            for elem in node:
                self.visit(elem)
        elif hasattr(node,
                     "children"):
            for child in node.children:
                if isinstance(child, list):
                    for item in child:
                        if isinstance(item, AST.Node):
                            self.visit(item)
                elif isinstance(child, AST.Node):
                    self.visit(child)


class TypeChecker(NodeVisitor):
    def __init__(self):
        self.table = SymbolTable(None, "root")
        self.actualType = ""
        self.actualLoop = False

    def visit_BinExpr(self, node):
        lewy = self.visit(node.left)
        prawy = self.visit(node.right)
        op = node.op
        if (lewy != None) and (prawy != None) and (type_dict[op][lewy][prawy] == None):
            print "Error: Illegal operation, {} {} {}: line {}".format(lewy, op, prawy, node.line)
        return type_dict[op][lewy][prawy]

    def visit_Integer(self, node):
        return 'int'

    def visit_Float(self, node):
        return 'float'

    def visit_String(self, node):
        return 'string'

    # dodalem to bo bez tego wali bardzo duzo None
    def visit_Variable(self, node):
        defn = self.table.getSymbol(node.name)
        if defn is None:
            #print self.table.getParentScope().entries
            #print "parent scope {}".format(self.table.getParentScope().entries)
            print "Error: Usage of undeclared variable '{}': line {}".format(node.name, node.line)
        elif isinstance(defn, FunctionSymbol):
            print "Error: Function identifier '{}' used as a variable: line {}".format(node.name, node.line)
        else:
            return defn.type

    def visit_ChoiInst(self, node):
        self.visit(node.cond)
        self.visit(node.inst)
        if node.alt is not None:
            self.visit(node.alt)

    def visit_WhiInst(self, node):
        self.visit(node.cond)
        self.actualLoop = True
        self.visit(node.inst)
        self.actualLoop = False

    def visit_RepInst(self, node):
        self.visit(node.cond)
        self.actualLoop = True
        self.visit(node.inst)
        self.actualLoop = False

    def visit_LabInst(self, node):
        self.visit(node.inst)

    def visit_PrtInst(self, node):
        self.visit(node.data)

    def visit_Program(self, node):
        self.visit(node.program)

    def visit_Podprogram(self, node):
        self.visit(node.podprogram)

    def visit_Unit(self, node):
        self.visit(node.unit)

    # done
    def visit_CompInst(self, node):
		#Poprawic scopy przy funkcjach
        self.table = SymbolTable(self.table, "scope")
        self.visit(node.units)
        self.table = self.table.getParentScope()

    # done
    def visit_RetInst(self, node):
        if self.actualFun is None:
            print "Error: return instruction outside a function: line {}".format(node.line)
        else:
            self.actualFun.returnExists = True
            exprType = self.visit(node.exp)
            if (exprType != None) and (self.actualFun.type != exprType):
                print "Error: Improper returned type, expected {}, got {}: line {}".format(self.actualFun.type, exprType, node.line)
                
    def visit_Continue(self, node):
        if self.actualLoop == False:
            print "Error: continue instruction outside a loop: line {}".format(node.line)

        
    def visit_Break(self, node):
        if self.actualLoop == False:
            print "Error: break instruction outside a loop: line {}".format(node.line)


    # done
    def visit_AsgInst(self, node):
        symbol = self.table.getSymbol(node.id)
        if symbol is None:
                print "Error: Variable '{}' undefined in current scope: line {}".format(node.id, node.line)
        defined_type = self.visit(node.exp)
        if (defined_type is not None) and (symbol != None):
            if symbol.type == "int" and defined_type == "float":
                print "warning: float -> int, losing precision, line {}".format(node.line)
            elif defined_type != symbol.type and (defined_type != "float" and symbol.type != "int") \
                    and (defined_type != "int" and symbol.type != "float"):
                print "Invalid assigment, wrong types, {} -> {}, line".format(defined_type, symbol.type, node.line)
        
    # done
    def visit_Init(self, node):
        defined_type = self.visit(node.exp)
        funSymbol = self.table.getSymbol(node.id)
        varSymbol = self.table.get(node.id)
        if isinstance(funSymbol, FunctionSymbol):
            print "Error: Function identifier '{}' used as a variable: line {}".format(node.id, node.line)
        elif varSymbol is None:
            if defined_type == self.actualType or (defined_type == "int" and self.actualType == "float"):
                self.table.put(node.id, VariableSymbol(node.id, self.actualType))
            elif defined_type == "float" and self.actualType == "int":
                print "warning: float -> int, losing precision in line {}".format(node.line)
                self.table.put(node.id, VariableSymbol(node.id, self.actualType))
            else:
                print "Error: Assignment of {} to {}: line {}".format(defined_type, self.actualType, node.line)
        else:
            print "Error: Variable '{}' already declared: line {}".format(node.id, node.line)

    def visit_Declaration(self, node):
        self.actualType = node.type
        self.visit(node.inits)
        self.actualType = ""

    def visit_Parameter(self, node):
        return node.type

    # done
    def visit_Function(self, node):
        if self.table.get(node.name) is None:
            fun = FunctionSymbol(node.name, node.type, SymbolTable(self.table, node.name))
            self.table.put(node.name, fun)
            self.actualFun = fun
            self.table = fun.table
            numOfParameter = 0
            for arg in node.parameters.children:
                self.visit(arg) #Wojtku, nie musisz pisac explicite visit_Parameter. Program sam wnioskuje, ktora funkcje odwiedzic, a jak nie to odwiedza defaultowa
                if self.table.get(arg.name) != None:
                    print "Error: Variable '{}' already declared: line {}".format(arg.name, node.line)
                varSym = VariableSymbol(arg.name, self.visit(arg))
                self.table.put(arg.name, varSym)
                fun.put(str(numOfParameter), varSym)
                numOfParameter = numOfParameter + 1
                #Zamiast nazywac parametry funkcji bedziemy je numerowac

            self.visit(node.body)
            if self.actualFun.returnExists == False:
                print "Error: Missing return statement in function '{}' returning int: line {}".format(node.name, node.body.endline)
            self.table = self.table.getParentScope()
            self.actualFun = None

        else:
            print "Error: Redefinition of function '{}': line {}".format(node.name, node.line)

    # done
    def visit_CallExp(self, node):
        fun = self.table.getSymbol(node.name)
        if fun is None or not isinstance(fun, FunctionSymbol):
            print "Error: Call of undefined function '{}': line {}".format(node.name, node.line)
            return None
        else:
            if len(node.args.children) != len(fun.args):
                print "Error: Improper number of args in {} call: line {}".format(fun.name, node.line)
                                                                                                          
            numOfParameter = 0
            improperTypes = False
            for arg in node.args.children:
                if numOfParameter < len(fun.args):
                    given = self.visit(arg)
                    original = fun.args[str(numOfParameter)].type
                    if given != original:
                        improperTypes = True
                numOfParameter = numOfParameter + 1

            if improperTypes == True:
                print "Error: Improper type of args in {} call: line {}".format(node.name, node.line)


        return fun.type
