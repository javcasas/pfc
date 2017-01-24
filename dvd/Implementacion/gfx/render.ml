class virtual capa=
	object(self)
		inherit General.sujeto
	end;;

class c_capa3d ob cam=
	object(self)
		inherit capa
		val mutable objeto_origen:Objetos.objeto_origen=ob
		method objeto_origen=objeto_origen
		method set_objeto_origen ob=
			objeto_origen <- ob
		
		val mutable camara:Objetos.camara=cam
		method camara=camara
		method set_camara cam=
			if cam#origen == objeto_origen then
				camara <- cam
			else
				failwith "capa3d#set_camara : la cámara no tiene de origen el objeto origen"
		initializer self#set_camara cam
	end;;

let contador_capa3d:c_capa3d General.agregador_observado=new General.agregador_observado;;
class capa3d ob cam=
	object(self)
		inherit c_capa3d ob cam
		initializer contador_capa3d#agregar (self:>c_capa3d)
	end;;


class c_capa_nula=
	object(self)
		inherit capa
	end;;



let contador_capa_nula:c_capa_nula General.agregador_observado=new General.agregador_observado;;
class capa_nula=
	object(self)
		inherit c_capa_nula
		initializer contador_capa_nula#agregar (self:>c_capa_nula)
	end;;
let capa_nula=new capa_nula;;


class c_capa_borrado=
	object(self)
		inherit capa
		val mutable color=new Geo.color_simple (new Mat.vector4 [|0.0;0.0;0.0;0.0|])
		method color=color
		method set_color c=
			color <- c
	end;;

let contador_capa_borrado:c_capa_borrado General.agregador_observado=new General.agregador_observado;;
class capa_borrado=
	object(self)
		inherit c_capa_borrado
		initializer contador_capa_borrado#agregar (self:>c_capa_borrado)
	end;;

class c_capa_borrado_zbuffer=
	object(self)
		inherit capa
	end;;

let contador_capa_borrado_zbuffer:c_capa_borrado_zbuffer General.agregador_observado=new General.agregador_observado;;
class capa_borrado_zbuffer=
	object(self)
		inherit c_capa_borrado_zbuffer
		initializer contador_capa_borrado_zbuffer#agregar (self:>c_capa_borrado_zbuffer)
	end;;

class c_escena (numcapas:int)=
	(* Primera capa: 1*)
	object(self)
		val capas=Array.make numcapas (capa_nula:>capa)
		method capas=capas
		method asignar_capa c i=
			capas.(i-1) <- c
		method capa i=
			capas.(i-1)
	end;;

let contador_escena:c_escena General.agregador_observado=new General.agregador_observado;;
class escena (numcapas:int)=
	object(self)
		inherit c_escena numcapas
		initializer contador_escena#agregar (self:>c_escena)
	end;;

class virtual c_pantalla=
	object(self)
		inherit General.sujeto
		method virtual dimensiones : int*int
		val mutable escena:escena option=None
		method set_escena e=
			escena <- Some(e);
			self#notificar
		method escena=escena
	end;;

let contador_pantalla:c_pantalla General.agregador_observado=new General.agregador_observado;;

type tmodo_pantalla=
	|Ventana
	|Pantalla_completa;;

type testado_ventana=
	|Primer_plano
	|Segundo_plano
	|Minimizada;;

class virtual pantalla=
	object(self)
		inherit c_pantalla
		initializer contador_pantalla#agregar (self:>c_pantalla)
	end;;

class monitor=
	object(self)
		inherit pantalla
		val mutable dim_x=800
		val mutable dim_y=600
		val mutable estado=Ventana
		val mutable estado_ventana=Primer_plano
		method dimensiones=dim_x, dim_y
		method set_dimensiones x y=
			dim_x <- x;
			dim_y <- y

		method estado=estado
		method set_estado m=
			estado <- m

		method cambiar_modo x y m=
			dim_x <- x;
			dim_y <- y;
			estado <- m;
			self#notificar

		method estado_ventana=estado_ventana
		method set_estado_ventana e=
			estado_ventana <- e
	end;;

let monitor_principal=new monitor;;
contador_pantalla#agregar (monitor_principal:>pantalla);;

class virtual motor_render=
	object(self)
		method virtual obten_pantallas:(string*pantalla) list
	end;;
