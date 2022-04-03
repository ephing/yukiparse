
STARTNONTERM = "\31"
EOF = "\30"
start = ""
semstack = []
table = {}
actions = {}
errorSets = {}
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
        global semstack, errorSets
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
                f = None
                state = list(filter(lambda x: x != '', self.stack[0].split(" ")))
                if state[-1] == "●":
                    f = str(errorSets["f" + state[0]])
                else:
                    f = str(errorSets[state[state.index("●") + 1]])
                print("Parse error " + str(self.tkstream[self.lookahead]['pos']) \
                    + ": Found " + str(self.tkstream[self.lookahead]['symbol']).replace('\x18', '\\x18') \
                    + ":" + str(self.tkstream[self.lookahead]['lexeme']) + ", Expected " + f)
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
