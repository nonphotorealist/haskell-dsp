HC=		ghc-5.04.3
HCOPTS=		-cpp

GC=		green-card

COPTS=		

GC_PATH=	/usr/local/lib/green-card
GC_LIBS=	$(GC_PATH)/StdDIS.o

GSL_PATH=	/usr/local/lib
GSL_INC=	/usr/local/include
#GSL_LIBS=	-lgsl -lgslcblas
GSL_LIBS=	-lgsl -lcblas -latlas

TARGET=		ffi

GCSRCS=		Airy.gc
GCSRCS+=	Bessel.gc
GCSRCS+=	Clausen.gc
GCSRCS+=	Ellint.gc
GCSRCS+=	Elljac.gc
GCSRCS+=	Erf.gc

OBJS=		$(GCSRCS:.gc=.o) $(HSSRCS:.hs=.o)

.SUFFIXES:
.SUFFIXES:	.o .hs .gc

.gc.o:
	$(GC) -t $(TARGET) -i $(GC_PATH) -o $@ $<
	$(HC) $(HCOPTS) -I. -I$(GSL_INC) -i$(GC_PATH) -package-name=Numeric -package lang -c $*.hs -o $*_hs.o
	$(HC) $(HCOPTS) -I. -I$(GSL_INC) -i$(GC_PATH) -package-name=Numeric -package lang -c $*_stub_$(TARGET).c -o $*_stub_$(TARGET).o
	$(LD) -r -o $@ $*_hs.o $*_stub_$(TARGET).o
#	$(RM) $*.hs 
	$(RM) $*_stub_$(TARGET).c $*_stub_$(TARGET).h
	$(RM) $*_hs.o $*_stub_$(TARGET).o

all:	$(OBJS)
#	$(HC) -package lang -o q q.hs $(OBJS) $(GC_LIBS) -L$(GSL_PATH) $(GSL_LIBS)

clean:
	rm -f *~ *.o *.hi

realclean: clean
	rm -f $(GCSRCS:.gc=.hs)
	rm -f $(GCSRCS:.gc=_stub_$(TARGET).c)
	rm -f $(GCSRCS:.gc=_stub_$(TARGET).h)
