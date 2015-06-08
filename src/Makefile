#
# 
#$Id: Makefile,v 1.2 2004/07/01 00:41:33 miyamoto Exp $ 
#
INSTALLDIR = ../work
GRACEDIR = /home/soft/grace/pro
GRACEDIR = /proj/soft/Release/1.00/grace

BASESOBJS = bsmain.o

SPRINGOBJS = spmain.o up4mas.o mnjhad.o

OBJS = mnjblk.o mnjcrs.o  mnjgam.o mnjgen.o \
	mnjmpl.o mnjprb.o \
	mnjtit.o mnjuin.o pystga.o \
	uvzero.o ubstbk.o ubstfd.o \
	getpar.o 

INSTALLSRC=mnj_bases.exe mnj_spring.exe run_mnjbas run_mnjspr

FFLAGS =-fdollar-ok -fno-backslash -fno-automatic -fno-f90 -fugly-complex \
	-fno-globals -fugly-init -Wno-globals \
	-fno-second-underscore

LIBS = -L$(GRACEDIR)/lib -lbases \
	-L$(CERN_ROOT)/lib -ljetset74 -lmathlib -lpacklib -lnsl -lm -lc

all: mnj_bases.exe mnj_spring.exe

mnj_bases.exe: $(OBJS) $(BASESOBJS)
	$(FC) -o mnj_bases.exe $(BASESOBJS) $(OBJS) $(LIBS)

mnj_spring.exe: $(OBJS) $(SPRINGOBJS)
	$(FC) -o mnj_spring.exe $(SPRINGOBJS) $(OBJS) $(LIBS)

install: $(INSTALLSRC)
	install -m 755 $(INSTALLSRC) $(INSTALLDIR)

clean: 
	rm -f $(OBJS) $(BASESOBJ) $(SPRINGOBJS) *.exe




