from scanner import Scanner
import AST
import TreePrinter



class Cparser(object):

    def __init__(self):
        self.scanner = Scanner()
        self.scanner.build()

    tokens = Scanner.tokens

    precedence = (
       ("nonassoc", 'IFX'),
       ("nonassoc", 'ELSE'),
       ("right", '='),
       ("left", 'OR'),
       ("left", 'AND'),
       ("left", '|'),
       ("left", '^'),
       ("left", '&'),
       ("nonassoc", '<', '>', 'EQ', 'NEQ', 'LE', 'GE'),
       ("left", 'SHL', 'SHR'),
       ("left", '+', '-'),
       ("left", '*', '/', '%'),
    )

    def p_error(self, p):
        if p:
            print("Syntax error at line {0}, column {1}: LexToken({2}, '{3}')".format(p.lineno, self.scanner.find_tok_column(p), p.type, p.value))
        else:
            print("Unexpected end of input")

    def p_program(self, p):
        """program : podprogramy"""
        p[0] = AST.Program(p[1])

    def p_podprogramy(self, p):
        """podprogramy : podprogramy podprogram
                        | """
        if len(p) == 3:
            podprogram = p[2]
            if p[1] == None:
                p[0] = AST.Podprogramy()
            else:
                p[0] = p[1]
            p[0].addPodprogram(podprogram)
        else:
            p[0] = AST.Podprogramy()

    def p_podprogram(self, p):
        """podprogram : declaration
                        | fundef
                        | instruction
                        | """
        p[0] = AST.Podprogram(p[1])

    def p_declaration(self, p):
        """declaration : TYPE inits ';' 
                       | error ';' """
        if len(p) == 4:
            type = p[1]
            inits = p[2]
            p[0] = AST.Declaration(type, inits)
        else:
            p[0] = AST.Error(p[1])

    def p_inits(self, p):
        """inits : inits ',' init
                 | init """
        if len(p) == 4:
            if p[1] == None:
                p[0] = AST.InitList(p[3])
            else:
                p[0] = p[1]
                p[0].addInit(p[3])
        else:
            p[0] = AST.InitList(p[1])

    def p_init(self, p):
        """init : ID '=' expression """
        id = p[1]
        exp = p[3]
        p[0] = AST.Init(id, exp, p.lineno(1))
    
    def p_instructions(self, p):
        """instructions : instruction instructions
                        | instruction """
        
        if len(p) == 3:
            if p[2] == None:
                p[0] = AST.InstList(p[1])
            else:
                p[0] = p[2]
                p[0].addInst(p[1])
        else:
            p[0] = AST.InstList(p[1])    

    def p_instruction(self, p):
        """instruction : print_instr
                       | labeled_instr
                       | assignment
                       | choice_instr
                       | while_instr 
                       | repeat_instr 
                       | return_instr
                       | break_instr
                       | continue_instr
                       | compound_instr
                       | expression ';' """
        p[0] = p[1]

    def p_print_instr(self, p):
        """print_instr : PRINT expr_list ';'
                       | PRINT error ';' """
        data = p[2]
        p[0] = AST.PrtInst(data, p.lineno(1))

    def p_labeled_instr(self, p):
        """labeled_instr : ID ':' instruction """
        id = p[1]
        inst = p[3]
        p[0] = AST.LabInst(id, inst, p.lineno(1))

    def p_assignment(self, p):
        """assignment : ID '=' expression ';' """
        id = p[1]
        exp = p[3]
        p[0] = AST.AsgInst(id,exp, p.lineno(1))

    def p_choice_instr(self, p):
        """choice_instr : IF '(' condition ')' instruction  %prec IFX
                        | IF '(' condition ')' instruction ELSE instruction
                        | IF '(' error ')' instruction  %prec IFX
                        | IF '(' error ')' instruction ELSE instruction """
        cond = p[3]
        inst = p[5]
        if len(p) >= 8:
            alt = p[7]
        else:
            alt = None
        p[0] = AST.ChoiInst(cond,inst,alt)

    def p_while_instr(self, p):
        """while_instr : WHILE '(' condition ')' instruction
                       | WHILE '(' error ')' instruction """
        cond = p[3]
        inst = p[5]
        p[0] = AST.WhiInst(cond, inst)

    def p_repeat_instr(self, p):
        """repeat_instr : REPEAT instructions UNTIL condition ';' """
        inst = p[3]
        cond = p[5]
        p[0] = AST.RepInst(inst,cond)

    def p_return_instr(self, p):
        """return_instr : RETURN expression ';' """
        exp = p[2]
        p[0] = AST.RetInst(exp, p.lineno(1))

    def p_continue_instr(self, p):
        """continue_instr : CONTINUE ';' """
        p[0] = AST.Continue(p.lineno(1))

    def p_break_instr(self, p):
        """break_instr : BREAK ';' """
        p[0] = AST.Break(p.lineno(1))


    #Przypilnowac by instruction nie zawieral fundefu
    #stworzyc unit -> declaration | instruction
    #i jeszcze stworzyc kolejna co by miala liste unitow.
    #poczatek zmian
    
    def p_units(self, p):
        """units : units unit
                 | """
        if len(p) == 3:
            unit = p[2]
            if p[1] == None:
                p[0] = AST.Units()
            else:
                p[0] = p[1]
            p[0].addUnit(unit)
        else:
            p[0] = AST.Units()

    def p_unit(self, p):
        """unit : declaration
                | instruction 
                | """
        p[0] = AST.Unit(p[1]);
    
    
    #koniec zmian
    def p_compound_instr(self, p):
        """compound_instr : '{' units '}' """
        if len(p[2].children) == 0:
            p[0] = AST.CompInst(None, p.lineno(3))
        else:
            p[0] = AST.CompInst(p[2], p.lineno(3))
    
    def p_condition(self, p):
        """condition : expression"""
        p[0] = p[1]

    def p_const(self, p):
        """const : integer
                 | float
                 | string"""
        p[0] = p[1]
        
    def p_integer(self, p):
        """integer : INTEGER"""
        p[0] = AST.Integer(p[1], p.lineno(1))
        
    def p_float(self, p):
        """float : FLOAT"""
        p[0] = AST.Float(p[1], p.lineno(1))

    def p_string(self, p):
        """string : STRING"""
        p[0] = AST.String(p[1], p.lineno(1))

    def p_expression_id(self, p):
        """expression : ID"""
        p[0] = AST.Variable(p.lineno(1), p[1])
#H4X10R5K1 sposob na odronienie ID od const
    def p_expression(self, p):
        """expression : const
                      | expression '+' expression
                      | expression '-' expression
                      | expression '*' expression
                      | expression '/' expression
                      | expression '%' expression
                      | expression '|' expression
                      | expression '&' expression
                      | expression '^' expression
                      | expression AND expression
                      | expression OR expression
                      | expression SHL expression
                      | expression SHR expression
                      | expression EQ expression
                      | expression NEQ expression
                      | expression '>' expression
                      | expression '<' expression
                      | expression LE expression
                      | expression GE expression
                      | '(' expression ')'
                      | '(' error ')'
                      | ID '(' expr_list_or_empty ')'
                      | ID '(' error ')' """
        if len(p) == 2:
            p[0] = p[1]
        elif p[2] == '(' and p[1] != '(':
            function_name = p[1]
            parameters = p[3]
            p[0] = AST.CallExp(function_name, parameters, p.lineno(1))
        elif p[1] == '(':
            p[0] = p[2]
        else:
            left = p[1]
            operator = p[2]
            right = p[3]
            p[0] = AST.BinExpr(left, operator, right, p.lineno(2))

    def p_expr_list_or_empty(self, p):
        """expr_list_or_empty : expr_list
                              | """
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = None

    def p_expr_list(self, p):
        """expr_list : expr_list ',' expression
                     | expression """
        if len(p) == 4:
            if p[1] == None:
                p[0] = AST.ExpList(p[3])
            else:
                p[0] = p[1]
                p[0].addExp(p[3])
        else:
            p[0] = AST.ExpList(p[1])

    def p_fundef(self, p):
        """fundef : TYPE ID '(' args_list_or_empty ')' compound_instr """
        type = p[1]
        name = p[2]
        parameters = p[4]
        body = p[6]
        p[0] = AST.Function(type, name, parameters, body, p.lineno(1))

    def p_args_list_or_empty(self, p):
        """args_list_or_empty : args_list
                              | """
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = None

    def p_args_list(self, p):
        """args_list : arg ',' args_list 
                     | arg """
        if len(p) == 4:
            if p[3] == None:
                p[0] = AST.ArgList(p[1])
            else:      
                p[0] = p[3]
                p[0].addArgToList(p[1])
        else:
            p[0] = AST.ArgList(p[1])          

    def p_arg(self, p):
        """arg : TYPE ID """
        type = p[1]
        name = p[2]
        p[0] = AST.Parameter(type, name, p.lineno(1))
