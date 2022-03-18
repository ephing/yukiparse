#!/usr/bin/env python3
import sys, os
from shutil import copyfile
import dotter
import argparse
from build import *

# Entrypoint
def main():
    aparse = argparse.ArgumentParser(description="Yukiparse, a parser builder")
    aparse.add_argument('file', metavar="GRAMMAR", help="format: python3 yukiparse.py GRAMMAR")
    aparse.add_argument('-p', action='store_true',
                        help="create dot file representing the parser as a state machine")
    aparse.add_argument('-o', metavar="FILE", type=str,
                        help="name output file as FILE")
    aparse.add_argument('-l', metavar="LANGUAGE", type=str,
                        help="specify an output language\nCurrently supports:\n\tPython\n\tHaskell\n\tGo")
    args = aparse.parse_args()

    if len(sys.argv) < 2:
        aparse.print_help()
        exit(1)

    file = args.file
    outname = "parser.py" if args.o is None else args.o
    dot = args.p
    lang = 'python' if args.l is None else args.l.lower()
    dir = "." if outname.rfind("/") == -1 else outname[:outname.rfind("/")]
    if dir != "." and not os.path.exists(dir):
        os.mkdir(dir)

    if lang not in ['python', 'haskell', 'go']:
        aparse.print_help()
        exit(1)
    elif lang == 'python':
        with open(dir + "/parse.py", "w") as outfile:
            if dir == '.': outfile.write("import lexer as lexer")
            else: outfile.write("import " + dir + ".lexer as lexer")
            with open("parsers/parse.py", "r") as parsepy:
                for line in parsepy.readlines():
                    outfile.write(line)
                
    elif lang == 'haskell':
        copyfile("parsers/parse.hs", dir + "/parse.hs")
    elif lang == 'go':
        pass

    buildParser(file, outname, lang)
    if dot:
        dotter.makedot(outname + ".dot")

if __name__ == '__main__':
    main()
