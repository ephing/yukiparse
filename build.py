import SLR
import Reader as r

def buildParser(filename, outname: str, lang: str):
    r.readGrammar(filename)
    seed = SLR.Seed(r.STARTNONTERM,[],r.grammar.prods[r.STARTNONTERM][0])
    SLR.State(seed)
    t = SLR.table()
    if lang.lower() == 'python':
        buildPython(outname, t)
    elif lang.lower() == 'haskell':
        buildHaskell(outname, t)
    elif lang.lower() == 'go':
        buildGo()

def recTablePrintPython(table, indent: int) -> str:
    out = ""
    for x in table:
        out += ("\t" * indent) + "\"" + x.replace('\x19','\\x19').replace('\x18','\\x18') + "\": "
        if isinstance(table[x], str):
            out += "\"" + table[x].replace('\x19','\\x19').replace('\x18','\\x18') + "\","
        elif isinstance(table[x], tuple):
            out += str(table[x]) + ","
        elif table[x] == None:
            out += "None,"
        else:
            out += "{\n" + recTablePrintPython(table[x], indent + 1) + ("\t" * indent) + "},"
        out += "\n"
    return out

def buildPython(filename, table):
    with open(filename,"w+") as file:
        file.write("#!/usr/bin/env python3\n")
        file.write("import parse as p\n")
        for line in r.requires:
            file.write(line + "\n")
        file.write("\n")

        # actual parser table
        file.write("p.table = {\n")
        file.write(recTablePrintPython(table, 1))
        file.write("}\np.start = list(p.table.keys())[0]\n")

        for a in r.idToAction:
            file.write("def action" + a[1:] + "():\n")
            for line in r.idToAction[a]:
                file.write(line + "\n")
            file.write("\n")

        # semantic action mappings
        file.write("p.actions = {\n")
        for a in r.prodToAction:
            file.write("\t\"" + a + "\": action" + r.prodToAction[a][1:] + ",\n")
        file.write("}\n")

def tablePrintHaskell(table) -> str:
    out = ""
    stateToHash = dict(map(lambda x: (x, str(abs(hash(x)))), table.keys()))
    out += "table :: String -> (String -> Parser.Transition)\n"
    for state in table:
        s = state.replace('\x19','\\x19').replace('\x18', '\\x18')
        out += "table \"" + s + "\" = state" + stateToHash[state] + "\n"
    out += "table _  = \\x -> Parser.None\n\n"

    for state in table:
        funcName = "state" + stateToHash[state]
        out += funcName + " :: String -> Parser.Transition\n"
        for input in table[state]:
            i = input.replace('\x19','\\x19').replace('\x18', '\\x18')
            t = table[state][input]
            if t == "accept":
                out += funcName + " \"" + i + "\" = Parser.Accept\n"
            elif isinstance(t, str):
                t = t.replace('\x19','\\x19').replace('\x18', '\\x18')
                out += funcName + " \"" + i + "\" = Parser.Shift \"" + t + "\"\n"
            elif isinstance(t, tuple):
                t2 = t[0].replace('\x19','\\x19').replace('\x18', '\\x18')
                out += funcName + " \"" + i + "\" = Parser.Reduce \"" + t2 + "\" " + str(t[1]) + "\n"
        out += funcName + " _ = Parser.None\n\n"
    return out

def buildHaskell(filename, table):
    with open(filename,"w+") as file:
        file.write("{-# LANGUAGE GADTs, FlexibleContexts #-}\n")
        file.write("import Parser\nimport Control.Monad\n\n")
        file.write(tablePrintHaskell(table))

def buildGo():
    pass