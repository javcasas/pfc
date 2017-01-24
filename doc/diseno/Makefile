all: base.tex intro arq det
	latex base
	dvipdfm base

intro:
	cd introduccion && $(MAKE)

arq:
	cd arquitectura && $(MAKE)

det:
	cd detallado && $(MAKE)

clean:
	rm -f base.aux base.dvi base.log base.pdf *~ 
	cd introduccion && $(MAKE) clean
	cd arquitectura && $(MAKE) clean
	cd detallado && $(MAKE) clean
