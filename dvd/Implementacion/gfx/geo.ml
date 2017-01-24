open Mat;;

class virtual coordenada=
	object(self)
		method virtual valor : vector3
	end;;

class virtual normal=
	object(self)
		method virtual valor : vector3
	end;;

class virtual color=
	object(self)
		method virtual valor : vector4
	end;;

class virtual uv=
	object(self)
		method virtual valor : vector2
	end;;

class virtual vertice=
	object(self)
		method virtual coordenada : coordenada
		method virtual normal : normal
		method virtual color : color
		method virtual uvs : uv list
	end;;

class virtual textura=
	object(self)
	end;;

type t_modificador_color_vertice=
	|Ambiente
	|Difuso
	|Especular
	|Emision
	|Ambiente_y_difuso;;

class virtual material=
	object(self)
		method virtual color_ambiente : color
		method virtual color_difuso : color
		method virtual color_especular : color
		method virtual brillo : float
		method virtual color_emision : color
		method virtual textura : textura
		method virtual modificador_color_vertice : t_modificador_color_vertice option
	end;;

class virtual capa=
	object(self)
		method virtual material:material
	end;;

class virtual poligono=
	object(self)
		method virtual vertices : vertice list
		method virtual capas: capa list
	end;;

class virtual malla=
	object(self)
		method virtual poligonos : poligono list
		method virtual vertices : vertice list
		method virtual materiales : material list
	end;;

class virtual bitmap=
	object(self)
		method virtual dim_x : int
		method virtual dim_y : int
		method virtual bits_por_pixel : int
		method virtual contenido : string
		method virtual get_pixel : int*int -> color
		method virtual formato_color : int32 * int32 * int32 * int32
	end;;

(* --------------------------------------- Malla simple --------------------------------------- *)

class coordenada_simple (v:vector3)=
	object(self)
		inherit coordenada
		val valor=v
		method valor=valor
	end;;

class normal_simple (v:vector3)=
	let v2=v#a_unitario in
	object(self)
		inherit normal
		val valor=v2
		method valor=valor
	end;;

class color_simple (v:vector4)=
	object(self)
		inherit color
		val valor=v
		method valor=v
	end;;

class uv_simple (v:vector2)=
	object(self)
		inherit uv
		val valor=v
		method valor=v
	end;;

class vertice_simple (c:coordenada_simple) (n:normal_simple) (co:color_simple) (uv:uv_simple list)=
	object(self)
		inherit vertice
		val coordenada=c
		val normal=n
		val color=co
		val uvs=uv
		method coordenada=coordenada
		method coordenada_simple=coordenada
		method normal=normal
		method normal_simple=normal
		method color=color
		method color_simple=color
		method uvs=uvs
		method uv_simples=uvs
	end;;
		

class c_textura_bitmap (f:string)=
	object
		inherit textura
		val fichero=f
		method fichero=fichero
		val mutable bitmap=(None:bitmap option)
		method bitmap=bitmap
		method set_bitmap b=
			bitmap <- Some(b)
	end;;

let contador_textura_bitmap : c_textura_bitmap General.agregador_observado=new General.agregador_observado;;

class textura_bitmap (f:string)=
	object(self)
		inherit c_textura_bitmap f
		initializer contador_textura_bitmap#agregar (self:>c_textura_bitmap)
	end;;

class c_no_textura=
	object(self)
		inherit textura
	end;;

let contador_no_textura : c_no_textura General.agregador_observado=new General.agregador_observado;;

class no_textura=
	object(self)
		inherit c_no_textura
		initializer contador_no_textura#agregar (self:>c_no_textura)
	end;;

class material_simple (c_ambiente:color)(c_difuso:color) ((c_especular,ang_especular):color*float) (c_emision:color) (t:textura)=
	object
		inherit material

		val mutable color_ambiente=c_ambiente
		method color_ambiente = color_ambiente
		method cambia_color_ambiente nco=
			color_ambiente <- nco
		
		val mutable color_difuso=c_difuso
		method color_difuso = color_difuso
		method cambia_color_difuso nco=
			color_difuso <- nco

		val mutable color_especular=c_especular
		method color_especular = color_especular
		method cambia_color_especular nco=
			color_especular <- nco

		val mutable brillo=ang_especular
		method brillo=brillo
		method cambia_brillo nb=
			brillo <- nb

		val mutable color_emision=c_emision
		method color_emision = color_emision
		method cambia_color_emision nco=
			color_emision <- nco

		val mutable modificador_color_vertice=None
		method modificador_color_vertice=modificador_color_vertice
		method cambia_modificador_color_vertice nmo=
			modificador_color_vertice <- nmo

		val mutable textura=t
		method textura = textura
		method cambia_textura nt=
			textura <- nt
	end;;

class capa_simple (m:material_simple)=
	object(self)
		inherit capa
		val mutable material=m
		method cambia_material nm=
			material <- nm
		method material=(material:>material)
		method material_simple=material
	end;;

class poligono_simple (v:vertice_simple list) (c:capa_simple list)=
	object
		inherit poligono
		val vertices=v
		method vertices=(vertices:>vertice list)
		method vertices_simples=vertices
		val capas=c
		method capas=(capas:>capa list)
		method capas_simples=capas
	end;;

class c_malla_simple (p:poligono_simple list) (v:vertice_simple list) (m:material_simple list)=
	object(self)
		inherit malla
		val poligonos=p
		method poligonos=(poligonos:poligono_simple list:>poligono list)
		method poligonos_simples=poligonos
		val vertices=v
		method vertices=(vertices:vertice_simple list:>vertice list)
		method vertices_simples=vertices
		val materiales=m
		method materiales=(materiales:material_simple list:>material list)
		method materiales_simples=materiales
	end;;

let contador_malla_simple : c_malla_simple General.agregador_observado=new General.agregador_observado;;
class malla_simple (p:poligono_simple list) (v:vertice_simple list) (m:material_simple list)=
	object(self)
		inherit c_malla_simple p v m
		initializer contador_malla_simple#agregar (self:>c_malla_simple)
	end;;


class virtual cargador_malla=
	object(self)
		method virtual cargar_malla : Ficheros.dispositivo_entrada_caracteres -> malla
	end;;
(*
let fabrica_color r g b a=
	let fr=(float_of_int r) /. 255.0 in
	let fg=(float_of_int g) /. 255.0 in
	let fb=(float_of_int b) /. 255.0 in
	let fa=(float_of_int a) /. 255.0 in
	new color_simple (new Mat.vector4 [|fr;fg;fb;fa|]);;


class bitmap_RGB (dx:int) (dy:int) (d:string)=
	let ()=
		if String.length d != (dx*dy*3) then
			failwith "bitmap_RGB: la cadena no coincide con las dimensiones especificadas"
		else
			() in
	object(self)
		inherit bitmap
		val dim_x=dx
		val dim_y=dy
		val contenido=d
		method dim_x = dim_x
		method dim_y = dim_y
		method bits_por_pixel = 24
		method contenido = contenido
		method get_pixel (x,y) =
			if (x >= 0) && (dim_x > x) && (y >= 0) && (dim_y > y) then
				let pixel = String.sub contenido (((y*dim_y)+x)*3) 3 in
				let r=int_of_char pixel.[0] in
				let g=int_of_char pixel.[1] in
				let b=int_of_char pixel.[2] in
				fabrica_color r g b 255
			else
				failwith "bitmap_RGB#get_pixel: pixel fuera del bitmap"			
		method formato_color=
			(Int32.of_int (255 lsl 16),
			Int32.of_int (255 lsl 8),
			Int32.of_int 255,
			Int32.of_int 0)
	end;;
		
class bitmap_RGBA (dx:int) (dy:int) (d:string)=
	let ()=
		if String.length d != (dx*dy*4) then
			failwith "bitmap_RGBA: la cadena no coincide con las dimensiones especificadas"
		else
			() in
	object(self)
		inherit bitmap
		val dim_x=dx
		val dim_y=dy
		val contenido=d
		method dim_x = dim_x
		method dim_y = dim_y
		method bits_por_pixel = 32
		method contenido = contenido
		method get_pixel (x,y) =
			if (x >= 0) && (dim_x > x) && (y >= 0) && (dim_y > y) then
				let pixel=String.sub contenido (((y*dim_y)+x)*4) 4 in
				let r=int_of_char pixel.[0] in
				let g=int_of_char pixel.[1] in
				let b=int_of_char pixel.[2] in
				let a=int_of_char pixel.[3] in
				fabrica_color r g b a
			else
				failwith "bitmap_RGBA#get_pixel: pixel fuera del bitmap"			
		method formato_color=
			(Int32.of_int (255 lsl 24),
			Int32.of_int (255 lsl 16),
			Int32.of_int (255 lsl 8),
			Int32.of_int 255)
	end;;*)
