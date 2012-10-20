LIBS=		Matrix
LIBS+=		Polynomial
LIBS+=		Numeric
LIBS+=		DSP

APPS=		FFTBench
APPS+=		FFTTest
APPS+=		Article
APPS+=		IIRDemo
APPS+=		FreqDemo
APPS+=		NoiseDemo

#OPT=		-O -funbox-strict-fields
#OPT+=		-fvia-C -O2-for-C

#PROF=		-prof -auto-all

#WARN=		-W
#WARN=		-Wall

#HEAP=		-H128m

HC=		ghc-5.04.3
#HC=		ghc-6.0

GC=		green-card

GC_PATH=	/usr/local/lib/green-card
GC_LIBS=	$(GC_PATH)/StdDIS.o

GSL_PATH=	/usr/local/lib
GSL_INC=	/usr/local/include
#GSL_LIBS=	-lgsl -lgslcblas
GSL_LIBS=	-lgsl -lcblas -latlas

TARGET=		ffi

GCSRCS=		Numeric/Special/Airy.gc
GCSRCS+=	Numeric/Special/Bessel.gc
GCSRCS+=	Numeric/Special/Clausen.gc
GCSRCS+=	Numeric/Special/Ellint.gc
GCSRCS+=	Numeric/Special/Elljac.gc
GCSRCS+=	Numeric/Special/Erf.gc

GCOBJS=		$(GCSRCS:.gc=.o)

HSFLAGS=	-fno-glasgow-exts ${OPT} ${PROF} ${WARN} ${HEAP}
HSFLAGS+=	-cpp -fffi -package lang -I$(GSL_INC) -i$(GC_PATH) 

INSTALLDIR=	/usr/home/donadio/lib/hs

URL=		http://haskelldsp.sourceforge.net/

.SUFFIXES:
.SUFFIXES:	.o .hs .gc

.gc.o:
	$(GC) -t $(TARGET) -i $(GC_PATH)  $<
	$(HC) $(HSFLAGS) -I. -I$(GSL_INC) -i$(GC_PATH) -package-name=Numeric -package lang -c $*.hs -o $*_hs.o
	$(HC) $(HSFLAGS) -I. -I$(GSL_INC) -i$(GC_PATH) -package-name=Numeric -package lang -c $*_stub_$(TARGET).c -o $*_stub_$(TARGET).o
	$(LD) -r -o $@ $*_hs.o $*_stub_$(TARGET).o
#	$(RM) $*.hs 
	$(RM) $*_stub_$(TARGET).c $*_stub_$(TARGET).h
	$(RM) $*_hs.o $*_stub_$(TARGET).o

all:	foreign libs

apps:
	for f in ${APPS}; do \
		$(HC) --make ${HSFLAGS} -o $$f demo/$$f.hs ;\
	done

foreign: $(GCOBJS)

libs:
	for f in ${LIBS}; do \
		find $$f \( -name "*.lhs" -or -name "*.hs" \) -print | xargs $(HC) --make -package-name=$$f ${HSFLAGS} ;\
	done

docs:
	find ${LIBS} -name "*.hs" -print | xargs haddock -h -o doc -t "Haskell DSP Library" -s "${URL}"

install: #libs
	for f in ${LIBS}; do \
		rm -f libHS$$f.a HS$$f.o ;\
		find $$f -name "*.o" -print | xargs ar cqs libHS$$f.a ;\
		ld -r --whole-archive -o HS$$f.o libHS$$f.a ;\
		for i in `find $$f -name "*.hi" -print`; do \
			mkdir -p `dirname ${INSTALLDIR}/imports/$$i` ;\
			install -m 644 $$i ${INSTALLDIR}/imports/$$i ;\
		done ;\
		install -m 644 libHS$$f.a ${INSTALLDIR} ;\
		install -m 644 HS$$f.o    ${INSTALLDIR} ;\
		installdir=${INSTALLDIR} ghc-pkg -f mpd.conf -u < pkg/$$f.pkg ;\
	done

test: test.hs
	$(HC) -cpp -package lang -i$(GC_PATH) -L$(GSL_PATH) $(GSL_LIBS) --make -o test test.hs

snapshot:
	tar cfz haskelldsp-snapshot.tar.gz ${LIBS} demo doc \
		COPYING README TODO Makefile

clean:
	find . -name "*.o" -print | xargs rm -f
	find . -name "*.hi" -print | xargs rm -f
	find . -name "*~" -print | xargs rm -f
	rm -f $(GCSRCS:.gc=.hs)
	rm -f $(GCSRCS:.gc=_stub_$(TARGET).c)
	rm -f $(GCSRCS:.gc=_stub_$(TARGET).h)
	rm -f *.a
	rm -f *.prof

realclean: clean
	rm -f ${APPS}
	rm -f *.core
	rm -f haskelldsp-snapshot.tar.gz
