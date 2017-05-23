
class Symbol():
    pass


class FunctionSymbol(Symbol):

    def __init__(self, name, type, table):
        self.name = name
        self.type = type
        self.table = table
        self.args = {}
        self.returnExists = False

    def put(self, name, symbol): # put variable symbol under <name> entry
        self.args[name] = symbol


class VariableSymbol(Symbol):

    def __init__(self, name, type):
        self.name = name
        self.type = type


class SymbolTable(object):

    def __init__(self, parent, name): # parent scope and symbol table name
        self.parent = parent
        self.name = name
        self.entries = {}

    def put(self, name, symbol): # put variable symbol or fundef under <name> entry
        self.entries[name] = symbol

    def get(self, name): # get variable symbol or fundef from <name> entry
        try:
            return self.entries[name]
        except:
            return None

    def getParentScope(self):
        return self.parent

    def getSymbol(self, name):
        if self.get(name) is None:
            try:
                return self.getParentScope().getSymbol(name)
            except:
                return None
        else:
            return self.get(name)


