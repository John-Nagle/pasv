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
UNICODELDC = UNICODECORNERS + UNICODETEES + UNICODELINES + UNICODEARROWS
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
        
    def getc(self, row, col) :
        """
        Get char at given row and column.
        Return space if column out of range or row not loaded yet.
        This makes everything look as if surrounded by whitespace.
        """
        if (self.lines[row] is None) :      # off the top
            return(" ")
        if col < 0 or col >= len(self.lines[row]) :
            return(" ")                     # off the end
        ch = self.lines[row][col]           # get char
        if ch == '\n' :                     # make end of line
            ch = " "                        # look like space
        return(ch)                          # normal case
        
                
    def fixchar(self, i) :
        """
        Fix one char in the middle line
        """
        #   Horizontals
        ch = self.getc(1,i)
        if ch not in "_-|+*<>^V" :
            return(ch)                      # uninteresting
        return("█")                         # ***TEMP****
        
    def fixline(self) :
        """
        Use Unicode line drawing symbols, examining a 3x3 square
        around the character of interest.  There is an extra space
        at the beginning and end of the line, to simplify indexing.
        The prevous line has already been processed.
        """
        s = ''
        for i in range(len(self.lines[1])) :# for all on line
            s += self.fixchar(i)            # do one char
        return(s)
        
    def addline(self, s) :                  # add and process one line
        self.lines[0] = self.lines[1]       # shift lines up by 1
        self.lines[1] = self.lines[2]
        self.lines[2] = s
        if self.lines[0] is not None :      # if all 3 lines are full
            s = self.fixline()              # fix middle line based on adjacent info
            self.outf.write(s.rstrip() + '\n')  # output line       
    
    def flush(self) :                       # call at end to flush last line
        self.addline("")                    # force last line out
        

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
