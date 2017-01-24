#OCAMLMAKEFILE = OCamlMakefile
#
#SOURCES = general/ficheros.ml general/mat.ml gfx/geo.ml gfx/cargador/cargador_malla_parser.mly gfx/cargador/cargador_malla_lexer.mll gfx/cargador/cargador_malla.ml gfx/cargador/tga/tga.ml  main.ml
#RESULT  = main

#INCDIRS=+sdl +lablgl 
#LIBS=bigarray sdl lablgl
#TRASH=*~

#-include $(OCAMLMAKEFILE)
default: bc

bc: main.exe
nc: main_opt.exe

run: main.exe
	export OCAMLRUNPARAM=b
	./main.exe > stdout.txt 2> stderr.txt
	cat stdout.txt

runopt: main_opt.exe
	export OCAMLRUNPARAM=b
	./main_opt.exe > stdout.txt 2> stderr.txt
	cat stdout.txt

OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep
#OCAMLLOC=$(shell ocamlc -where)
OCAMLLOC=d:/cygwin/ocamlmgw/lib
INCLUDES=-I general -I gfx -I opengl -I procesos #-I $(OCAMLLOC)/sdl -I $(OCAMLLOC)/lablgl
INCLUDES_LIBS=-I $(OCAMLLOC)/sdl -I $(OCAMLLOC)/lablGl
OCAMLFLAGS=$(INCLUDES) $(INCLUDES_LIBS) -thread -g    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) $(INCLUDES_LIBS) -thread # add other options for ocamlopt here

BIBLIOTECAS=unix threads bigarray sdl sdlloader lablgl

FUENTES=general/registro general/mat general/ficheros general/general general/recursos general/procesos gfx/geo gfx/bitmaps gfx/cargador_malla_lexer gfx/cargador_malla_parser gfx/cargador_malla gfx/objetos gfx/render general/vista opengl/m_opengl logica main

BC_OBJS=$(addsuffix .cmo, $(FUENTES))
BC_BIBS=$(addsuffix .cma, $(BIBLIOTECAS))

main.exe: $(BC_OBJS)
	$(OCAMLC) -custom -o main.exe $(OCAMLFLAGS) $(BC_BIBS) $(BC_OBJS) /src/ocamlsdl-0.7.2/src/mlsdl_main.o -cclib -lSDL



NC_OBJS=$(addsuffix .cmx, $(FUENTES))
NC_BIBS=$(addsuffix .cmxa, $(BIBLIOTECAS))

main_opt.exe: $(NC_OBJS)
	$(OCAMLOPT) -o main_opt.exe $(OCAMLOPTFLAGS) $(NC_BIBS) $(NC_OBJS) /src/ocamlsdl-0.7.2/src/mlsdl_main.o -cclib -lSDL

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.mll.ml:
	ocamllex $<

%.ml %.mli: %.mly
	ocamlyacc $<

# Clean up
.PHONY : clean 
clean:
	rm -f main.exe main_opt.exe
	rm -f *.cm[iox] *.o *~
	cd general && rm -f *.cm[iox] *.o *~
	cd gfx && rm -f *.cm[iox] *.o *~
	cd gfx && rm -f cargador_malla_lexer.ml cargador_malla_parser.ml cargador_malla_parser.mli
	cd opengl && rm -f *.cm[iox] *.o *~

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) -slash *.mli *.ml general/*.mli general/*.ml gfx/*.mli gfx/*.ml opengl/*.ml opengl/*.mli > .depend

include .depend
gfx/cargador_malla.cmo: gfx/cargador_malla_parser.cmo gfx/cargador_malla_lexer.cmo
gfx/cargador_malla.cmx: gfx/cargador_malla_parser.cmx gfx/cargador_malla_lexer.cmx
gfx/cargador_malla_lexer.cmo: gfx/cargador_malla_parser.cmi
gfx/cargador_malla_lexer.cmx: gfx/cargador_malla_parser.cmi
gfx/cargador_malla_parser.cmo: gfx/cargador_malla_parser.cmi
gfx/cargador_malla_parser.cmx: gfx/cargador_malla_parser.cmi

