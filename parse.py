import ..lexer as lexer
STARTNONTERM = "\31"
EOF = "\30"
start = ""
semstack = []
table = {}
actions = {}
currentToken = None

class Parser:
    def __init__(self,tks: str):
        self.stack = [start]
        self.lookahead = 0
        self.tkstream = lexer.lex.start(tks)
        global semstack, currentToken
        semstack = []
        currentToken = None

    def parse(self):
        global semstack
        while len(self.stack) != 0:
            next = table[self.stack[0]][self.tkstream[self.lookahead]['symbol']]
            #accept
            if next == "accept":
                if len(semstack)>0: return semstack[0]
                else: 
                    print("CRINGE! Parse error: empty sem stack")
                    exit(1)
            #failure
            elif next == None:
                print("Parse error: cringe token " + str(self.tkstream[self.lookahead]))
                exit(1)
            #shift/goto
            elif isinstance(next,str):
                self.stack = [next] + self.stack
                self.lookahead += 1
                global currentToken
                currentToken = self.tkstream[self.lookahead - 1]["lexeme"]
            #reduce
            else:
                if self.stack[0] in actions.keys():
                    actions[self.stack[0]]()
                self.stack = [table[self.stack[next[1]]][next[0]]] + self.stack[next[1]:]
        print("Parse Error: unexpected EOF")
        exit(1)
