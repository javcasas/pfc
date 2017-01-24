class cargador=
	object(self)
		inherit Geo.cargador_malla
		method cargar_malla_simple (f:Ficheros.dispositivo_entrada_caracteres)=
			let lector cad n=
				let texto,caracteres=f#leer_cadena n in
				String.blit texto 0 cad 0 caracteres;
				caracteres in
			let lexbuf=Lexing.from_function lector in
			let objeto,bitmaps=Cargador_malla_parser.main Cargador_malla_lexer.token lexbuf in
			objeto,bitmaps
		method cargar_malla (f)=
			let malla,bitmaps=self#cargar_malla_simple f in
			((malla):Geo.malla_simple :> Geo.malla)
	end;;

let micargador=new cargador;

class r_cargador_malla_simple filename=
	object(self)
		inherit [Geo.malla_simple] Recursos.cargador
		val mutable malla=None
		val mutable bitmaps=None
		method cargar=
			let fichero=new Ficheros.fichero_entrada filename in
			let m,b=micargador#cargar_malla_simple fichero in
			malla <- Some(m);
			bitmaps <- Some(b);
			let carga_bitmap b=
				Bitmaps.gestor_bitmaps#cargar b in
			List.iter carga_bitmap b;
			self#cambia_estado Recursos.Cargado
		method descargar=
			malla <- None;
			bitmaps <- None;
			self#cambia_estado Recursos.No_cargado
		method obtener=
			let Some(m)=malla in
			m
		method clonar=
			self#obtener
	end;;

class instanciador_malla_simple filename=
	let cargador = new r_cargador_malla_simple filename in
	object(self)
		inherit [Geo.malla_simple, Geo.malla_simple] Recursos.instanciador cargador
		method instanciar a=
			a
	end;;

let gestor_mallas_simples : (Geo.malla_simple Recursos.gestor_recursos) = new Recursos.gestor_recursos;;
