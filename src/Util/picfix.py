#   coding=utf-8
#
#   picfix -- Line drawing picture converter
#
#   Converts pictures written in ASCII using * - | < - > _ + to line drawing
#   characters in Unicode.
#
#   John Nagle
#   January, 2017
#
import argparse
import sys
#
#   Constants
#
UNICODECORNERS = "┏┓┗┛"
UNICODETEES = "┣┫┻┳"
UNICODELINES = "┃━"
UNICODEARROWS = "▶◀▲▼"
#
#   Globals
#
verbose = False                 # verbose mode

#
#   class Linegroup -- group of three lines handled together
#
class Linegroup :
    def __init__(self, outf) :
        self.outf = outf                # output goes here
        self.lines = [None, None,None]  # last three lines
        
    def addline(self, s) :              # add and process one line
        if self.lines[0] is not None :
            self.outf.write(self.lines[0])
        self.lines[0] = self.lines[1]   # shift lines by 1
        self.lines[1] = self.lines[2]
        self.lines[2] = s
    
    def flush(self) :                   # call at end to flush last line
        for i in range(2) :
            if self.lines[i] is not None :
                self.outf.write(self.lines[i])
        

#
#   dofile -- do one file
#
def dofile(infilename) :
    outf = sys.stdout                   # ***TEMP***
    with open(infilename, 'r') as infile :
        lwork = Linegroup(outf)         # line group object
        for line in infile :
            lwork.addline(line)
        lwork.flush()
                   
    
#
#   main -- main program
#
def main() :
    parser = argparse.ArgumentParser(description='Fix pictures in text files')
    parser.add_argument('-v', "--verbose", action='store_true', help='Verbose')
    parser.add_argument("FILE", help="Text file to process", nargs="+")
    args = parser.parse_args()
    verbose = args.verbose              # set verbosity
    for filename in args.FILE :         # do each file       
        dofile(filename)
    
    
    
#
main()
