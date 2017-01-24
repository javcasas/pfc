all: base.tex intro prop
	latex base
	dvipdfm base

intro:
	cd introduccion && $(MAKE)

prop:
	cd propuesto && $(MAKE)

clean:
	rm -f base.aux base.dvi base.log base.pdf *~ 
	cd introduccion && $(MAKE) clean
	cd propuesto && $(MAKE) clean
