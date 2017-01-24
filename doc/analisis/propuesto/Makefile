all: propuesto.tex vis_general req_funcionales req_no_funcionales mod_del_sistema esc casos_uso mod_dinamicos mod_objeto

vis_general:
	cd vision_general && $(MAKE)

req_funcionales:
	cd requisitos_funcionales && $(MAKE)

req_no_funcionales:
	cd requisitos_no_funcionales && $(MAKE)

mod_del_sistema:
	cd modelos_del_sistema && $(MAKE)

esc:
	cd escenarios && $(MAKE)

casos_uso:
	cd modelo_de_casos_de_uso && $(MAKE)

mod_dinamicos:
	cd modelos_dinamicos && $(MAKE)

mod_objeto:
	cd modelos_objeto && $(MAKE)

clean:
	rm -f *~
	cd vision_general && $(MAKE) clean
	cd requisitos_funcionales && $(MAKE) clean
	cd requisitos_no_funcionales && $(MAKE) clean
	cd modelos_del_sistema && $(MAKE) clean
	cd escenarios && $(MAKE) clean
	cd modelo_de_casos_de_uso && $(MAKE) clean
	cd modelos_dinamicos && $(MAKE) clean
	cd modelos_objeto && $(MAKE) clean
