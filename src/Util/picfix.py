#   coding=utf-8
#
#   picfix -- Line drawing picture converter
#
#   Python 3
#
#   Converts pictures written in ASCII using * - | < - > _ + to line drawing
#   characters in Unicode.
#
#   For now, output is always to standard output.
#
#   John Nagle
#   January, 2017
#
#
import argparse
import sys
#
#   Constants
#
#
#   Line drawing chars
#
#           0123456789abcdef
# N          N N N N N N N N
# S           SS  SS  SS  SS
# E             EEEE    EEEE
# W                 WWWWWWWW  
BOXCHARS = "XXX┃X┗┏┣X┛┓┫━┻┳╋"            # line drawing chars, by NSEW bits   

ARROWCHARS ="▲▼▶◀"                      # arrow chars, NSEW order
ARROWSUBST = {'^': '▲', 'v' : '▼', 'V' : '▼', '>' : '▶', '<' : '◀' } # arrow substitutions  ➡
####ARROWSUBST = {'^': '▲', 'v' : '↓', 'V' : '↓', '>' : '➤', '<' : '◀' } # arrow substitutions  ➡
#
#   Globals
#
verbose = False                         # verbose mode

#
#   popcount - population count
#
def popcount(n) :
    assert(n >= 0)                      # nonnegative, or we will hang
    if n == 0 :
        return(0)
    if n & 1 :
        return(1 + popcount(n >> 1))
    return(popcount(n >> 1))            # if only we had tail recursion
    
#
#   expandtabs  -- expand tabs to spaces
#
def expandtabs(s, tabspacing) :
    if tabspacing is None or tabspacing <= 0 : # no tabs
        return(s)                           # do nothing
    pos = 0                                 # output position
    sout = ''
    for ch in s :                           # for all chars
        if ch == '\t' :                     # for a tab
            while True                  :   # fill to next tab pos
                sout += ' '                 # with spaces
                pos += 1
                if pos % tabspacing == 0 :  # at tab pos
                    break                   # quit
        else :
            sout += ch
            pos += 1
    return(sout)
   

#
#   class Linegroup -- group of three lines handled together
#
class Linegroup :
    def __init__(self, outf, nodraw) :
        self.outf = outf                    # output goes here
        self.nodraw = nodraw                # don't add line drawing chars, just fix tabs
        self.lines = [None, None,None, None, None]  # last five lines
        
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
        
    def validarrow(self, row, col) :
        """
        Is char at row, col a valid arrow?
        
        Arrows are ^ V < >.
        They must have white space on either side.
        They must have a suitable line character leading to the arrow
        """
        ch = self.getc(row, col)
        if ch in "^vV" :
            if self.getc(row,col-1) != ' ' or self.getc(row,col+1) != ' ' :
                return(False)               # whitespace check fail
            if ch == '^' and self.getc(row-1, col) in '|' :
                return(True)
            if ch in "vV" and self.getc(row-1, col) in '|' :
                return(True)
        if ch in "<>" :
            if self.getc(row-1,col) != ' ' or self.getc(row+1,col) != ' ' :
                return(False)               # whitespace check fail
            if ch == '>' and self.getc(row, col-1) in '-_' :
                return(True)
            if ch == '<' and self.getc(row, col+1) in '-_' :
                return(True)
        return(False) 
                
    def fixchar(self, i) :
        """
        Fix one char in the middle line, which is line 2
        """
        ch = self.getc(2,i)
        if ch not in "_-|+*<>^V" :
            return(ch)                      # uninteresting
        #   Line recognition
        neighbors = 0                       # neighbor bits NSEW
        if self.getc(1,i) in "|-_+*" or (self.getc(1,i) == '^' and self.validarrow(1,i)) :  # if north valid
            neighbors |= 1                  # north bit
        if self.getc(3,i) in "-_|+*" or (self.getc(3,i) == 'V' and self.validarrow(3,i)) : # if south valid
            neighbors |= 2                  # south bit
        if self.getc(2,i+1) in "|-_+*" or (self.getc(2,i+1) == '>' and self.validarrow(2,i+1)) : # if east valid
            neighbors |= 4                  # south bit
        if self.getc(2,i-1) in "|-_+*" or (self.getc(2,i-1) == '<' and self.validarrow(2,i-1)) :  # if west valid
            neighbors |= 8                  # south bit        
        if ch in "|-_+*" and popcount(neighbors) > 1 :# ***TEMP***
            return(BOXCHARS[neighbors])  
        #   Arrow recognition
        if self.validarrow(2,i) :           # if valid arrow
            ch = ARROWSUBST[ch]             # subsitute arrow char  
        return(ch)                          # no change
        
    def fixline(self) :
        """
        Use Unicode line drawing symbols, examining a 5x5 square
        around the character of interest.  
        """
        s = ''
        for i in range(len(self.lines[2])) :# for all on line
            s += self.fixchar(i)            # do one char
        return(s)
        
    def addline(self, s) :                  # add and process one line
        self.lines[0] = self.lines[1]       # we use the last 5 lines
        self.lines[1] = self.lines[2]
        self.lines[2] = self.lines[3]       # shift lines up by 1
        self.lines[3] = self.lines[4]
        self.lines[4] = s
        if self.lines[0] is not None :      # if all 5 lines are full
            if self.nodraw :
                s = self.lines[2]           # don't fix line
            else :
                s = self.fixline()          # fix middle line based on adjacent info
            self.outf.write(s.rstrip() + '\n')  # output line       
    
    def flush(self) :                       # call at end to flush last line
        self.addline("")                    # force last line out
        self.addline("")
        

#
#   dofile -- do one file
#
def dofile(infilename, tabval, nodraw) :
    outf = sys.stdout                       # for now, output is always stdout
    with open(infilename, 'r') as infile :
        lwork = Linegroup(outf, nodraw)     # line group object
        for line in infile :
            lwork.addline(expandtabs(line, tabval))
        lwork.flush()
                          
    
#
#   main -- main program
#
def main() :
    parser = argparse.ArgumentParser(description='Fix pictures in text files')
    parser.add_argument('-v', "--verbose", action='store_true', help='Verbose')
    parser.add_argument('-t', "--tab", dest="tabval", metavar='N', type=int, default=4, help='Spaces per tab')
    parser.add_argument('-n', "--nodraw", dest="nodrawval", action='store_true', help="Don't insert drawing chars")
    parser.add_argument("FILE", help="Text file to process", nargs="+")
    args = parser.parse_args()
    verbose = args.verbose              # set verbosity
    for filename in args.FILE :         # do each file       
        dofile(filename, args.tabval, args.nodrawval)
    
    
    
#
main()
