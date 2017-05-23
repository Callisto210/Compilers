class Memory:
    def __init__(self, name):  # memory name
        self.name = name
        self.space = {}

    def has_key(self, name):  # variable name
        return name in self.space

    def get(self, name):  # gets from memory current value of variable <name>
        if self.has_key(name):
           return self.space[name]
        return None

    def put(self, name, value):  # puts into memory current value of variable <name>
        self.space[name] = value


class MemoryStack:
    def __init__(self, memory=None):  # initialize memory stack with memory <memory>
        self.stack = []
        if memory is None:
            self.stack.append(Memory("Top"))
        else:
            self.stack.append(memory)

    def get(self, name):  # gets from memory stack current value of variable <name>
        size = len(self.stack)-1
        
        while size >= 0 :
            if self.stack[size].get(name) is not None:
               # print "got " + name + " from : " + self.stack[size].name
                return self.stack[size].get(name)
            else:
                #print "get " + name + " from : " + self.stack[size].name
                if self.stack[size].name == "function":
                    return self.stack[0].get(name)
                else:
                    size -= 1
        return None


    def insert(self, name, value):  # inserts into memory stack variable <name> with value <value>
        self.stack[len(self.stack) - 1].put(name, value)

    def set(self, name, value):  # sets variable <name> to value <value>
        size = len(self.stack)-1
        
        while size >= 0 :
            if self.stack[size].get(name) is not None:
                #print "sett " + name + " in : " + self.stack[size].name
                return self.stack[size].put(name, value)
            else:
                #print "set " + name + " in : " + self.stack[size].name
                if self.stack[size].name == "function":
                    return self.stack[0].put(name, value)
                else:
                    size -= 1
        # TO DO to wyzej zmieniamy tylko zmienna w 1 przestrzeni czy wyzej tez?? mysle ze taka zmienna moze byc tylko raz

    def push(self, memory):  # pushes memory <memory> onto the stack
        self.stack.append(memory)

    def pop(self):  # pops the top memory from the stack
        return self.stack.pop()

    def lastSpace(self):
        return self.stack[-1]

