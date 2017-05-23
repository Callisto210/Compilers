 
class Node(object):
    def __str__(self):
        return self.printTree()
        
    def accept(self, visitor):
        return visitor.visit(self)
    pass

class BinExpr(Node):
    def __init__(self, left, op, right, line):
        self.op = op
        self.left = left
        self.right = right
        self.line = line

class Variable(Node):
    def __init__(self, line, name):
        self.name = name
        self.line = line

class Const(Node):
    def __init__(self, value, line):
        self.value = value
        self.line = line

class Integer(Const):
    pass

class Float(Const):
    pass

class String(Const):
    pass

class ExpList(Node):

    def __init__(self, exp):
        self.children = [exp]

    def addExp(self, expr):
        self.children.append(expr)

class CallExp(Node):
    def __init__(self, name, args, line):
        self.name = name
        self.args = args
        self.line = line

class Function(Node):
    def __init__(self, type, name, parameters, body, line):
        self.type = type
        self.name = name
        self.parameters = parameters
        self.body = body
        self.line = line
        

class ArgList(Node):
    def __init__(self, arg):
        self.children = [arg]

    def addArgToList(self, arg):
        self.children.append(arg)


class Parameter(Node):
    def __init__(self, type, name, line):
        self.type = type
        self.name = name
        self.line = line


class Program(Node):
    def __init__(self, program):
        self.program = program


class Podprogramy(Node):
    def __init__(self):
        self.children = []

    def addPodprogram(self, arg):
        self.children.append(arg)


class Podprogram(Node):
    def __init__(self, podprogram):
        self.podprogram = podprogram


class Declaration(Node):
    def __init__(self, type, inits):
        self.type = type
        self.inits = inits


class Error(Node):
    def __init__(self, msg):
        self.msg= msg


class InitList(Node):
    def __init__(self, init):
        self.children = [init]

    def addInit(self, init):
        self.children.append(init)


class Init(Node):
    def __init__(self, id, exp, line):
        self.id = id
        self.exp = exp
        self.line = line


class InstList(Node):
    def __init__(self, inst):
        self.children = [inst]

    def addInst(self, inst):
        self.children.insert(0, inst)


class PrtInst(Node):
    def __init__(self, data, line):
        self.data = data
        self.line = line


class LabInst(Node):
    def __init__(self, id, inst, line):
        self.id = id
        self.inst = inst
        self.line = line


class AsgInst(Node):
    def __init__(self, id, exp, line):
        self.id = id
        self.exp = exp
        self.line = line


class ChoiInst(Node):
    def __init__(self, cond, inst, alt):
        self.cond = cond
        self.inst = inst
        self.alt = alt


class WhiInst(Node):
    def __init__(self, cond, inst):
        self.inst = inst
        self.cond = cond


class RepInst(Node):
    def __init__(self, inst, cond):
        self.inst = inst
        self.cond = cond


class RetInst(Node):
    def __init__(self, exp, line):
        self.exp = exp
        self.line = line


class Continue(Node):
    def __init__(self, line):
        self.line = line


class Break(Node):
    def __init__(self, line):
        self.line = line


class CompInst(Node):
    def __init__(self, units, endline):
        self.units = units
        self.endline = endline
        
    def accept(self, visitor, calledBy = None):
        return visitor.visit(self, calledBy)


class Units(Node):
    def __init__(self):
        self.children = []

    def addUnit(self, arg):
        self.children.append(arg)

class Unit(Node):
    def __init__(self, unit):
        self.unit = unit
