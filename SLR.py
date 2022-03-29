import Reader as r

states = {}

class Seed:
    def __init__(self,z,a,b):
        #LHS
        self.z = z
        #already seen
        self.a = a
        #to be seen
        self.b = b
    
    def __repr__(self) -> str:
        return self.z + " := " + ' '.join(self.a) + " ● " + ' '.join(self.b)

    def getNext(self) -> str:
        if self.b != []: return self.b[0]

class State:
    def __init__(self,seed: Seed):
        global states
        self.items = [seed]
        self.outTokens = {}
        self.final = seed.b == [r.EOF]
        temp = []
        #gotta run until saturation or something
        while self.items != temp:
            temp = list(map(lambda x: x,self.items))
            #do closures on everything in self.items
            for i in temp:
                if i.getNext() != None and i.getNext() in r.grammar.nonterms:
                    for p in r.grammar.prods[i.getNext()]:
                        s = Seed(i.getNext(),[],p)
                        if str(s) not in list(map(lambda x: str(x), self.items)):
                            self.items += [s]

        states.update({str(seed): self})
        #Generate all connected states
        for x in r.grammar.nonterms + r.grammar.terms: self.getNext(x)
        
    #add item to list and update all values
    def merge(self,s: Seed):
        self.items += [s]
        temp = []
        #gotta run until saturation or something
        while self.items != temp:
            temp = list(map(lambda x: x,self.items))
            #do closures on everything in self.items
            for i in temp:
                if i.getNext() != None and i.getNext() in r.grammar.nonterms:
                    for p in r.grammar.prods[i.getNext()]:
                        s = Seed(i.getNext(),[],p)
                        for x in self.items:
                            if str(x) == str(s):
                                break
                        else:
                            self.items += [s]
        
        if s.getNext() != None: self.getNext(s.getNext())

    def getNext(self,token: str):
        global states
        f = None
        for i in self.items:
            if i.getNext() == token and i.getNext() != r.EOF:
                #make new seed with dot moved over 1
                s = Seed(i.z,i.a + [i.b[0]], i.b[1:])
                if f == None:
                    f = s
                    #do not allow for duplicate states
                    if str(s) in states: 
                        self.outTokens.update({token: list(states.values()).index(states[str(s)])})
                        continue
                    #create new state with new seed
                    State(s)
                else:
                    #sometimes states close on a symbol in multiple productions
                    #this makes it so it adds to the current state its building instead
                    #of generating a whole new state
                    if str(s) not in map(lambda x: str(x), states[str(f)].items):
                        states[str(f)].merge(s)
        if f != None: 
            self.outTokens.update({token: list(states.values()).index(states[str(f)])})
            return states[str(f)]

    def getName(self):
        return str(self.items[0])

def table():
    res = {states[s].getName():{t:None for t in r.grammar.terms + r.grammar.nonterms} for s in states}
    for s in states:
        #shifts and gotos, represented by the seed of the next state
        for o in states[s].outTokens:
            if res[states[s].getName()][o] == None:
                res[states[s].getName()][o] = states[s].getNext(o).getName()
            else:
                if isinstance(res[states[s].getName()][o], str):
                    print("Shift/Shift error in state \'" + states[s].getName() + "\' with token " + o)
                else:
                    print("Shift/Reduce error in state \'" + states[s].getName() + "\' with token " + o)
                exit(1)
        #accept, represented by word "accept"
        if states[s].final:
            if res[states[s].getName()][r.EOF] == None:
                res[states[s].getName()][r.EOF] = "accept"
            else:
                print("Grammar somehow terminates before the EOF")
                exit(1)
        #reduces, represented by (LHS,number of things to pop off stack)
        for i in states[s].items:
            if i.b == []:
                for x in r.grammar.followSets[i.z]:
                    if res[states[s].getName()][x] == None:
                        res[states[s].getName()][x] = (i.z,len(i.a))
                    else:
                        if isinstance(res[states[s].getName()][o], str):
                            print("Shift/Reduce error in state \'" + states[s].getName() + "\' with token " + o)
                        else:
                            print("Reduce/Reduce error in state \'" + states[s].getName() + "\' with token " + o)
                        exit(1)
                        exit(1)
    return res
