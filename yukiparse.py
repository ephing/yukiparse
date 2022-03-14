#!/usr/bin/env python3
import Reader as r
import sys
import SLR
import dotter
import argparse

def recTablePrint(table, indent: int) -> str:
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
            out += "{\n" + recTablePrint(table[x], indent + 1) + ("\t" * indent) + "},"
        out += "\n"
    return out

def buildParser(filename, outname: str):
    r.readGrammar(filename)
    seed = SLR.Seed(r.STARTNONTERM,[],r.grammar.prods[r.STARTNONTERM][0])
    SLR.State(seed)
    t = SLR.table()
    with open(outname,"w+") as file:
        file.write("#!/usr/bin/env python3\n")
        file.write("import parse as p\n")
        for line in r.requires:
            file.write(line + "\n")
        file.write("\n")

        # actual parser table
        file.write("p.table = {\n")
        file.write(recTablePrint(t, 1))
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

# Entrypoint
def main():
    aparse = argparse.ArgumentParser(description="Yukiparse, a parser builder")
    aparse.add_argument('file', metavar="GRAMMAR", help="format: python3 yukiparse.py GRAMMAR")
    aparse.add_argument('-p', action='store_true',
                        help="create dot file representing the parser as a state machine")
    aparse.add_argument('-o', metavar="FILE", type=str,
                        help="name output file as FILE")
    args = aparse.parse_args()

    if len(sys.argv) < 2:
        aparse.print_help()
        exit(1)

    file = args.file
    outname = "parser.py" if args.o is None else args.o
    dot = args.p

    buildParser(file, outname)
    if dot:
        dotter.makedot(outname + ".dot")

if __name__ == '__main__':
    main()
