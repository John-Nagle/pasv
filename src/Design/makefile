#		Makefile for verifier internal documentation
#
#	    John Nagle
#       January, 2017
#
#   Currently uses "groff" to make PDF files.
#
TARGET=.
GROFF=groff
GOPTIONS=-m mm -Tpdf

%.pdf: %.mm $(DEPS)
	$(GROFF) $(GOPTIONS) $< > $(TARGET)/$@

all:    cpci1.pdf cpci1.pdf cpci2.pdf cpci3.pdf cpci4.pdf cpci5.pdf cpci6.pdf \
icode.pdf icodechg.pdf installbm.pdf intro.pdf irdprop.pdf \
jcode.pdf jnotes.pdf jtovc.pdf practical.pdf restrict.pdf \
safemulti.pdf scott.pdf

cpci1.pdf:  cpci1.mm

cpci2.pdf:  cpci2.mm

cpci3.pdf:  cpci3.mm

cpci4.pdf:  cpci4.mm

cpci5.pdf:  cpci5.mm

cpci6.pdf:  cpci6.mm

icode.pdf:  icode.mm

icodechg.pdf:    icode.mm

installlbm.pdf: installlbm.mm

intro.pdf:  intro.mm

irdprop.pdf:    irdprop.mm

jcode.pdf:  jcode.mm

jnotes.pdf: jnotes.mm

jtovc.pdf: jtovc.mm

practical.pdf:  practical.mm

restrict.pdf:   restrict.mm

safemulti.pdf: safemulti.mm

scott.pdf:  scott.mm
