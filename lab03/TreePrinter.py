
import AST


def addToClass(cls):

    def decorator(func):
        setattr(cls,func.__name__,func)
        return func
    return decorator


class TreePrinter:
#Do printTree dokladam dodatkowy parametr mowiacy o zaglebieniu wywolan printTree
    @addToClass(AST.Node)
    def printTree(self, depth=0):
        raise Exception("printTree not defined in class " + self.__class__.__name__)

    @addToClass(AST.Program)
    def printTree(self, depth=0):
        return self.program.printTree(depth)
        
    @addToClass(AST.Podprogramy) #aka lista podprogramow
    def printTree(self, depth=0):
        return "".join(map(lambda node: node.printTree(depth), self.children))
        
    @addToClass(AST.Podprogram)
    def printTree(self, depth=0):
        return  ("| " * depth + "DECL" + "\n" + self.podprogram.printTree(depth+1) 
                if isinstance(self.podprogram, AST.Declaration)
                else self.podprogram.printTree(depth))
        
    @addToClass(AST.Parameter)
    def printTree(self, depth=0):
        return "| " * depth + "ARG " + str(self.name) + "\n"
        
    @addToClass(AST.ArgList) #lista parametrow
    def printTree(self, depth=0):
        return "".join(map(lambda node: node.printTree(depth), self.children))       
    
    @addToClass(AST.Function)
    def printTree(self, depth=0):
        return  "| " * depth + "FUNDEF" + "\n" + \
                "| " * (depth + 1) + str(self.name) + "\n" +\
                "| " * (depth + 1) + "RET " + str(self.type) + "\n" + \
                (self.parameters.printTree(depth+1)) + \
                (self.body.printTree(depth+1))
    
    @addToClass(AST.Declaration)
    def printTree(self, depth=0):
        return self.inits.printTree(depth)
        
    @addToClass(AST.Init)
    def printTree(self, depth=0):
        return  "| " * depth + "=" + "\n" + \
                "| " * (depth+1) + str(self.id) + "\n" + \
                self.exp.printTree(depth+1)
        
    @addToClass(AST.InitList)
    def printTree(self, depth=0):
        return "".join(map(lambda node: node.printTree(depth), self.children))

    @addToClass(AST.ExpList)
    def printTree(self, depth=0):
        return "".join(map(lambda node: node.printTree(depth), self.children))

    @addToClass(AST.CallExp)
    def printTree(self, depth=0):
        return  "| " * depth + "FUNCALL" + "\n" + \
                "| " * (depth+1) + str(self.name) + "\n" + \
                ("| " * (depth+1) + self.args if isinstance(self.args, str)
                else self.args.printTree(depth+1))
        
    @addToClass(AST.InstList)
    def printTree(self, depth=0):
        return "".join(map(lambda node: node.printTree(depth), self.children))
        
    @addToClass(AST.PrtInst)
    def printTree(self, depth=0):
        return  "| " * (depth-1) + "PRINT" + "\n" + \
                "| " * depth + str(self.data)
        
    @addToClass(AST.RetInst)
    def printTree(self, depth=0):
        return  "| " * (depth-1) + "RETURN" + "\n" + \
                self.exp.printTree(depth)
        
    @addToClass(AST.RepInst)
    def printTree(self, depth=0):
        return  "| " * (depth-1) + "REPEAT" + "\n" + \
                self.inst.printTree(depth) + \
                "| " * (depth-1) + "UNTIL" + "\n" + \
                self.cond.printTree(depth)
        
    @addToClass(AST.AsgInst)
    def printTree(self, depth=0):
        return  "| " * (depth) + "=" + "\n" + \
                "| " * (depth+1) + str(self.id) + "\n" + \
                self.exp.printTree(depth+1)
        
    @addToClass(AST.LabInst)
    def printTree(self, depth=0):
        return  "| " * depth + "LABEL" + "\n" + \
                "| " * (depth+1) + str(self.id) + "\n" + \
                self.inst.printTree(depth+1)
        
    @addToClass(AST.ChoiInst)
    def printTree(self, depth=0):
        return  "| " * (depth-1) + "IF" + "\n" + \
                self.cond.printTree(depth) + \
                self.inst.printTree(depth) + \
                ( "" if self.alt is None \
                    else "| " * (depth-1) + "ELSE" + "\n" + \
                    self.alt.printTree(depth)
                )
        
    @addToClass(AST.CompInst)
    def printTree(self, depth=0):
        return  (
                    "" if self.units is None
                    else  self.units.printTree(depth+1)
                )
    
    @addToClass(AST.WhiInst)
    def printTree(self, depth=0):
        return  "| " * depth + "WHILE" + "\n" + \
                self.cond.printTree(depth+1) + \
                self.inst.printTree(depth)
        
    @addToClass(AST.Continue)
    def printTree(self, depth=0):
        return "| " * depth + "CONTINUE" + "\n"
        
    @addToClass(AST.Break)
    def printTree(self, depth=0):
        return "| " * depth + "BREAK" + "\n"
        
    @addToClass(AST.Const)
    def printTree(self, depth=0):
        return "| " * depth + str(self.value) + "\n"
        
    @addToClass(AST.Variable)
    def printTree(self, depth=0):
        return "| " * depth + str(self.name) + "\n"

    @addToClass(AST.BinExpr)
    def printTree(self, depth=0):
        return  "| " * depth + str(self.op) + "\n" + \
                (self.left if isinstance(self.left, str)
                else self.left.printTree(depth+1)) + \
                self.right.printTree(depth+1)
                
    @addToClass(AST.Error)
    def printTree(self, depth=0):
        return  "| " * depth + "ERROR" + "\n" + \
                "| " * (depth+1) + str(self.msg) + "\n"


    @addToClass(AST.Units)
    def printTree(self, depth=0):
        return "".join(map(lambda node: node.printTree(depth), self.children))
        
    @addToClass(AST.Unit)
    def printTree(self, depth=0):
        return  ("| " * (depth-1) + "DECL" + "\n" + self.unit.printTree(depth) 
                if isinstance(self.unit, AST.Declaration)
                else self.unit.printTree(depth))
