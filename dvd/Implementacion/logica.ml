class manejador_orbita (padre:Objetos.objeto) (radio:float) (ang_inicial:float) (vel_rotacion:float) =
	object(self)
		inherit Procesos.proceso_abstract
		val mutable ang_actual = ang_inicial
		val dummy = new Objetos.dummy
		method dummy = dummy
		initializer
			dummy#emparentar padre
		method ejecutar =
			ang_actual <- ang_actual +. vel_rotacion;
			let px= (cos ang_actual) *. radio in
			let py= (sin ang_actual) *. radio in
			dummy#transformada#establecer_posicion (new Mat.vector3 [|px;py;0.0|])
	end;;

class planeta (orbita:manejador_orbita) (size:float) (m:Geo.malla) (rotacion_inicial:(Mat.vector3 * float)) (vector_rotacion:Mat.vector3) (ang_inicial:float) (vel_rotacion:float)=
	object(self)
		inherit Procesos.proceso_abstract
		val mutable ang_actual = ang_inicial
		val objeto = new Objetos.objeto_malla
		initializer
			objeto#set_malla m;
			let padre=orbita#dummy in
                        objeto#emparentar padre;
			objeto#transformada#establecer_escala (new Mat.vector3 [|size;size;size|])

		method ejecutar = 
			ang_actual <- ang_actual +. vel_rotacion;
			let vec, ang=rotacion_inicial in
			objeto#transformada#establecer_rotacion vec ang;
			objeto#transformada#rotar vector_rotacion ang_actual
	end;;



class manejador_camara (c:Objetos.camara_perspectiva)=
	object(self)
		inherit Procesos.proceso_abstract
		val camara=c
		val mutable ang_posx = 0.0
		val mutable ang_posy = 0.0
		val mutable ang_posz = 0.0
		method ejecutar=
			ang_posz <- ang_posz +. 0.001;
                        let posz = 22.0 +. ((sin ang_posz) *. 6.0) in

			ang_posy <- ang_posy +. 0.0013;
                        let posy = -22.0 +. ((sin ang_posy) *. 6.0) in

			ang_posx <- ang_posx +. 0.0016;
                        let posx = ((sin ang_posx) *. 6.0) in


			camara#transformada#establecer_posicion (new Mat.vector3 [|posx;posy;posz|]);

                        let angulox = atan2 (-.posy) (posz) in
                        let anguloy = atan2 (posx) (posz) in
                        let anguloz = 0.0 in
			camara#transformada#establecer_rotacion Mat.vector3_i angulox;
			camara#transformada#rotar Mat.vector3_j anguloy;
			camara#transformada#rotar Mat.vector3_k anguloz

	end;;



let constante_kepler=1000.0;;

Random.init 3;

class proceso_base=
	let genera_inicial =
		fun () -> Random.float (Mat.pi *. 2.0) in
	object(self)
		inherit Procesos.proceso_abstract
		method preparar_malla filename=
			let instanciador:Cargador_malla.instanciador_malla_simple:>(Geo.malla_simple Recursos.a_instanciador)=(new Cargador_malla.instanciador_malla_simple filename)   in
			Cargador_malla.gestor_mallas_simples#nuevo_instanciador filename instanciador;

		method leer_malla filename=
			self#preparar_malla filename;
			Cargador_malla.gestor_mallas_simples#forzar_obtener filename

		method crear_sol ((malla:Geo.malla_simple),radio) origen (velocidad_rotacion)=
			let orbita=new manejador_orbita origen 0.0 0.0 0.0 in
			self#lanzar (orbita :> Procesos.proceso_abstract);
			let sol = new planeta orbita radio (malla:>Geo.malla) (Mat.vector3_i, 0.0) (new Mat.vector3 [|0.0;0.0;1.0|]) 1.0 velocidad_rotacion in
			self#lanzar (sol :> Procesos.proceso_abstract);
			orbita

		method crear_planeta ((malla:Geo.malla_simple),radio) (padre,radio_orbita) (velocidad_rotacion)=
			let velocidad_translacion = 2.0 *. Mat.pi /. sqrt(constante_kepler *. radio_orbita *. radio_orbita *. radio_orbita) in
			let orbita=new manejador_orbita padre radio_orbita (genera_inicial()) velocidad_translacion in
			self#lanzar (orbita :> Procesos.proceso_abstract);
			let planeta = new planeta orbita radio (malla:>Geo.malla) (Mat.vector3_i, 0.0) (new Mat.vector3 [|0.0;0.0;1.0|]) 1.0 velocidad_rotacion in
			self#lanzar (planeta :> Procesos.proceso_abstract);
			orbita

		method ejecutar=
			Render.monitor_principal#cambiar_modo 1024 768 Render.Ventana;
			let malla_sol = self#leer_malla "datos/sol.mesh" in
			let malla_mercurio = self#leer_malla "datos/mercurio.mesh" in
			let malla_venus = self#leer_malla "datos/venus.mesh" in
			let malla_tierra = self#leer_malla "datos/tierra.mesh" in
				let malla_luna = self#leer_malla "datos/luna.mesh" in
			let malla_marte = self#leer_malla "datos/marte.mesh" in
			let malla_jupiter = self#leer_malla "datos/jupiter.mesh" in
				let malla_io = self#leer_malla "datos/io.mesh" in			
				let malla_ganimedes = self#leer_malla "datos/ganimedes.mesh" in			
				let malla_europa = self#leer_malla "datos/europa.mesh" in			
				let malla_calisto = self#leer_malla "datos/calisto.mesh" in			
			let malla_saturno = self#leer_malla "datos/saturno.mesh" in
				let malla_titan = self#leer_malla "datos/titan.mesh" in
			let malla_urano = self#leer_malla "datos/urano.mesh" in
			let malla_neptuno = self#leer_malla "datos/neptuno.mesh" in
			let malla_pluton = self#leer_malla "datos/pluton.mesh" in
				let malla_caronte = self#leer_malla "datos/caronte.mesh" in


			let origen=new Objetos.objeto_origen in
			
			let camara=new Objetos.camara_perspectiva in
                        camara#emparentar (origen:>Objetos.objeto);
(*			camara#establecer_padre (origen:>Objetos.objeto);
			origen#añadir_hijo (camara:>Objetos.objeto);*)
			camara#set_focal (0.1,0.1);

			let luz_sol=new Objetos.punto_luz in
			luz_sol#set_c_ambiente (new Geo.color_simple (new Mat.vector4 [|1.0;1.0;1.0;1.0|]));
			luz_sol#emparentar (origen:>Objetos.objeto);
			
			let capa_borrado=new Render.capa_borrado in
			let capa_borrado_zbuffer=new Render.capa_borrado_zbuffer in
			let capa3d=new Render.capa3d origen (camara:>Objetos.camara) in

			let escena=new Render.escena 3 in
			escena#asignar_capa (capa_borrado:>Render.capa) 1;
			escena#asignar_capa (capa_borrado_zbuffer:>Render.capa) 2;
			escena#asignar_capa (capa3d:>Render.capa) 3;

			Render.monitor_principal#set_escena escena;

			self#lanzar (new manejador_camara camara);

			let orbita_sol=self#crear_sol (malla_sol,3.0) origen 0.02 in

			let orbita_mercurio=self#crear_planeta (malla_mercurio, 0.3) (origen, 4.1) 0.04 in
			let orbita_venus=self#crear_planeta (malla_venus, 0.5) (origen, 5.6) 0.03 in
			let orbita_tierra=self#crear_planeta (malla_tierra, 1.0) (origen, 9.3) 0.3 in
				let orbita_luna=self#crear_planeta (malla_luna, 0.4) (orbita_tierra#dummy, 1.7) 0.3 in
			let orbita_marte=self#crear_planeta (malla_marte, 0.7) (origen, 12.5) 0.2 in
			let orbita_jupiter=self#crear_planeta (malla_jupiter, 2.0) (origen, 20.54) 0.1 in
				let orbita_io=self#crear_planeta (malla_io, 0.25) (orbita_jupiter#dummy, 20.54 -. 14.0) 0.02 in
				let orbita_ganimedes=self#crear_planeta (malla_ganimedes, 0.3) (orbita_jupiter#dummy, 20.54 -. 14.8) 0.02 in
				let orbita_europa=self#crear_planeta (malla_europa, 0.25) (orbita_jupiter#dummy, 20.54 -. 15.54) 0.02 in
				let orbita_calisto=self#crear_planeta (malla_calisto, 0.2) (orbita_jupiter#dummy, 20.54 -. 16.04) 0.02 in
			let orbita_saturno=self#crear_planeta (malla_saturno, 1.5) (origen, 33.58) 0.1 in
				let orbita_titan=self#crear_planeta (malla_titan, 0.5) (orbita_saturno#dummy, 33.58 -. 29.58) 0.1 in
			let orbita_urano=self#crear_planeta (malla_urano, 1.2) (origen, 38.98) 0.1 in
			let orbita_neptuno=self#crear_planeta (malla_neptuno, 1.1) (origen, 42.18) 0.1 in
			let orbita_pluton=self#crear_planeta (malla_pluton, 0.35) (origen, 44.88) 0.1 in
				let orbita_caronte=self#crear_planeta (malla_caronte, 0.3) (orbita_pluton#dummy, 44.88 -. 43.78) 0.1 in
			
			self#señal Procesos.Dormir
	end;;

let proceso_base:Procesos.proceso=(new proceso_base :> Procesos.proceso);;
