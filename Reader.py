# These two are used to add an extra nonterm, term, and production to the grammar
# \31 ::= X \30
# where X is the first nonterminal listed in the .ag file
# I use this to force the EOF token at the end of the grammar, for use in set generation
EOF = "\30"
STARTNONTERM = "\31"

class Grammar:
    def __init__(self,terms,nonterms,prods):
        self.terms = terms + [EOF]
        self.nonterms = nonterms + [STARTNONTERM]
        self.prods = prods
        self.prods.update({STARTNONTERM: [[self.nonterms[0], EOF]]})
        self.firstSets = {'':{None}}
        self.followSets = {EOF: {None}, STARTNONTERM: {None}}

        #term first sets
        for i in self.terms: self.firstSets.update({i: {i}})
        #nonterm first sets
        for n in self.nonterms: self.FIRST_sym(n)
        #symbol string first sets
        for p in self.prods:
            for x in self.prods[p]:
                if x == []: continue
                self.FIRST_prod(x)
        # follow sets for terms and nonterms
        for t in self.terms: 
            self.FOLLOW(t)
            #print(t, self.followSets)
        for n in self.nonterms: self.FOLLOW(n)

    def print(self) -> None:
        print("Term:",self.terms,"\nNonterm:",self.nonterms,"\nProds",self.prods)

    # get FIRST set of a single symbol
    def FIRST_sym(self, symbol: str) -> set:
        if symbol in self.firstSets: return self.firstSets[symbol]
        self.firstSets.update({ symbol: set({}) })
        
        for p in self.prods[symbol]:
            # production is epsilon, add epsilon to FIRST set
            if p == [] and None not in self.firstSets[symbol]:
                self.firstSets[symbol].add(None)
            else:
                # check symbols in production sequentially
                for i in p:
                    #get FIRST set of current symbol in production
                    recursionBabyWOOOOO = self.FIRST_sym(i)

                    # if we're at the end of the production or i is not nullable, add FIRST(i) including epsilon if its there
                    # do not continue through symbol string
                    if None not in recursionBabyWOOOOO or p.index(i) == len(p) - 1:
                        for x in recursionBabyWOOOOO:
                            if x not in self.firstSets[symbol]:
                                self.firstSets[symbol].add(x)
                        break
                    # if we get here, i was nullable, add FIRST(i) excluding epsilon and continue through symbol string
                    for x in recursionBabyWOOOOO:
                        if x != None and x not in self.firstSets[symbol]:
                            self.firstSets[symbol].add(x)
        return self.firstSets[symbol]

    # get FIRST set of a symbol string
    def FIRST_prod(self, prod: list) -> None:
        prodStr = ' '.join(prod)
        if prodStr in self.firstSets: return
        self.firstSets.update({prodStr: set({})})
        for s in prod:
            # add FIRST(s)
            for x in self.firstSets[s]:
                if x not in self.firstSets[prodStr]: self.firstSets[prodStr].add(x)
            # if s is nullable and there are still more symbols in the production
            # then remove epsilon from the current first set and add first set of the production excluding the first symbol
            #
            # if there is only one symbol left and epsilon in FIRST(s), then the entire production is nullable
            # we keep epsilon if its there and dont recurse
            if None in self.firstSets[s] and len(prod) > 1:
                #remove epsilon and recurse
                self.firstSets[prodStr].remove(None)
                self.FIRST_prod(prod[1:])
                for x in self.firstSets[' '.join(prod[1:])]:
                    if x not in self.firstSets[prodStr]:
                        self.firstSets[prodStr].add(x)
            else: break

    # get FOLLOW set of a single symbol
    def FOLLOW(self,symbol: str, prev: set = set({})) -> set:
        if symbol not in self.followSets: self.followSets.update({symbol: set({})})

        for n in self.prods:
            for p in self.prods[n]:
                if symbol not in p: continue
                for index in range(len(p)):
                    if p[index] != symbol: continue
                    followZ = None
                    # Beta is empty, calculate FOLLOW(Z)
                    if index == len(p) - 1 and n not in prev:
                        followZ = self.FOLLOW(n, prev.union({n}))
                    else:
                        # get FIRST(Beta) and add it to FOLLOW(symbol)
                        self.FIRST_prod(p[index+1:])
                        bStr = ' '.join(p[index+1:])
                        for f in self.firstSets[bStr]:
                            if f != None and f not in self.followSets[symbol]: 
                                self.followSets[symbol].add(f)
                        # Beta is nullable, calculate FOLLOW(Z)
                        if None in self.firstSets[bStr] and n not in prev: 
                            followZ = self.FOLLOW(n, prev.union({n}))
                    # add FOLLOW(Z) if it was calculated
                    if followZ != None:
                        for z in followZ:
                            if z not in self.followSets[symbol]: self.followSets[symbol].add(z)
        return self.followSets[symbol]

grammar = None
prodToAction = {}
idToAction = {}
requires = []
def readGrammar(filename):
    global grammar, prodToAction, idToAction, requires
    bnf = True
    currentNonterm = None
    terms = []
    nonterms = []
    prods = {}
    with open(filename,"r") as file:
        line = file.readline()
        while line != '':
            line = line.lstrip().rstrip().split(" ")
            while '' in line: line.remove('')
            if line == []: 
                line = file.readline()
                continue
            if bnf:
                if line == ["###"]:
                    bnf = False
                    line = file.readline()
                    continue
                if (line[0] != ''): 
                    if (line[0] != '|'):
                        currentNonterm = line[0]
                        if currentNonterm not in nonterms:
                            nonterms.append(currentNonterm)
                            if currentNonterm in terms: terms.remove(currentNonterm)
                            prods.update({currentNonterm: []})
                        prods[currentNonterm].append(list(filter(lambda x: x[0] != '#', line[2:])))
                        for i in range(2,len(line)):
                            if line[i][0] == '#':
                                prodToAction[' '.join(line[:i] + ["●"] + line[i+1:]) + " "] = line[i]
                            elif line[i] not in nonterms and line[i] not in terms:
                                terms.append(line[i])
                    else:
                        prods[currentNonterm].append(list(filter(lambda x: x[0] != '#', line[1:])))   
                        for i in range(1,len(line)):
                            if line[i][0] == '#':
                                prodToAction[' '.join([currentNonterm, ":="] + line[1:i] + ["●"] + line[i+1:]) + " "] = line[i]
                            elif line[i] not in nonterms and line[i] not in terms:
                                terms.append(line[i])             
                line = file.readline()
            else:
                if line == []: 
                    pass
                elif line[0] == 'requires':
                    while (line := file.readline().rstrip().lstrip()) != "}":
                        requires += [line]
                elif line[0][0] == '#':
                    if line[0] not in prodToAction.values():
                        raise Exception("Invalid semantic action: " + line[0])
                    act = line[0]
                    idToAction[act] = []
                    while (line := file.readline().rstrip()) != "}":
                        idToAction[act] += [line]
                line = file.readline()
    grammar = Grammar(terms,nonterms,prods)