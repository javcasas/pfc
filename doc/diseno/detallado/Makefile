all: detallado.tex intro deta

intro:
	cd introduccion && $(MAKE)

deta:
	cd diseno_detallado_de_las_clases && $(MAKE)

clean:
	rm -f *~
	cd introduccion && $(MAKE) clean
	cd diseno_detallado_de_las_clases && $(MAKE) clean
