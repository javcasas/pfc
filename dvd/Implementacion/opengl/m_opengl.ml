(******************************************************************************************************)
(* Clases generales para Motor OpenGL                                                                 *)
(******************************************************************************************************)
class virtual ['a] vigilante_agregador (ag:'a General.agregador_observado)=
	object(self)
		inherit General.observador
		val sujeto=ag
		method virtual nuevo_dibujador: 'a -> unit
		method virtual eliminar_dibujador: 'a -> unit			
		method actualizar=
			match sujeto#ultima_accion with
			|General.Agregar(a) -> self#nuevo_dibujador a
			|General.Quitar(a) -> self#eliminar_dibujador a
			
		initializer 
			sujeto#añadir_observador (self:>General.observador)
		method destruir=
			sujeto#eliminar_observador (self:>General.observador)
	end;;



(******************************************************************************************************)
(* Módulo Bitmap                                                                                      *)
(******************************************************************************************************)
class virtual dibujador_bitmap=
	(* Carga bitmaps en la memoria de OpenGL *)
	object(self)
		method virtual activar:unit
		method virtual destruir:unit
	end;;

class dibujador_bitmap_RGBA (b:Bitmaps.c_bitmap_RGBA)=
	object(self)
		inherit dibujador_bitmap
		val bitmap=b
		val mutable t_index=None
		method bitmap=bitmap
		method enviar_a_opengl=
			let raw=Raw.of_string bitmap#contenido `ubyte in
			let formato=match bitmap#bits_por_pixel with
				|24 -> `rgb
				|32 -> `rgba in
			let pix=GlPix.of_raw raw ~format:formato ~width:bitmap#dim_x ~height:bitmap#dim_y in
			let t=GlTex.gen_texture () in
			let ()=GlTex.bind_texture ~target:`texture_2d t in
			let ()=GlPix.store (`unpack_alignment 1) in
			let ()=GlTex.image2d pix in
			let ()=List.iter (GlTex.parameter ~target:`texture_2d)
					[	`wrap_s `repeat;
						`wrap_t `repeat;
						`mag_filter `linear;
						`min_filter `linear ] in
			let ()=GlTex.env (`mode `modulate) in
			Gl.raise_error "Bitmap";
			t_index <- Some(t)
		method activar=
			match t_index with
			|Some(t) -> 
				GlTex.bind_texture ~target:`texture_2d t;
				Gl.enable `texture_2d
			|None ->
				self#enviar_a_opengl;
				self#activar
		method destruir=
			match t_index with
			|Some(t) -> GlTex.delete_texture t;
						t_index <- None
			|None -> ()
	end;;


let asociador_bitmaps_dibujadores:(Geo.bitmap, dibujador_bitmap) General.asociador=new General.asociador;;



class vigilante_bitmap_RGBA (ag:Bitmaps.bitmap_RGBA General.agregador_observado)=
	let cast_bitmap m=
		(m:Bitmaps.bitmap_RGBA:>Geo.bitmap) in
	object(self)
		inherit [Bitmaps.bitmap_RGBA] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_bitmap_RGBA m in
			asociador_bitmaps_dibujadores#asociar (cast_bitmap m) (dibujador:>dibujador_bitmap)
		method eliminar_dibujador m=
			let dibujador=asociador_bitmaps_dibujadores#buscar_asociado (cast_bitmap m) in
			dibujador#destruir;
			asociador_bitmaps_dibujadores#quitar_asociacion (cast_bitmap m)
	end;;

(******************************************************************************************************)
(* Módulo Geo                                                                                         *)
(******************************************************************************************************)
class virtual dibujador_textura=
	object(self)
		method virtual activar:unit 
		method virtual destruir:unit
	end;;

(* textura_bitmap *)
(* El cargador de mallas lee los bitmaps de las texturas lo último *)
(* Por eso aquí implementamos una carga en OpenGL tardía, *)
(* sólo cuando se necesita *)
class dibujador_textura_bitmap (t:Geo.textura_bitmap)=
	object(self)
		inherit dibujador_textura
		val textura=t
		method textura=textura
		method activar=
			match textura#bitmap with
			|Some(a) -> (Gl.enable `texture_2d;
						let dib=asociador_bitmaps_dibujadores#buscar_asociado a in
						dib#activar)
			|None ->	(let bitmap=Bitmaps.gestor_bitmaps#forzar_obtener textura#fichero in
						textura#set_bitmap bitmap;
						self#activar)
		method destruir=
			()
			(*GlTex.delete_texture t_index*)
	end;;

(* no_textura *)
class dibujador_no_textura (t:Geo.no_textura)=
	object(self)
		inherit dibujador_textura
		method activar=
			Gl.disable `texture_2d;
		method destruir=()
	end;;

let asociador_texturas_dibujadores:(Geo.textura, dibujador_textura) General.asociador=new General.asociador;;

class vigilante_textura_bitmap (ag:Geo.textura_bitmap General.agregador_observado)=
	object(self)
		inherit [Geo.textura_bitmap] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_textura_bitmap m in
			asociador_texturas_dibujadores#asociar (m:Geo.textura_bitmap:>Geo.textura) (dibujador:>dibujador_textura)
		method eliminar_dibujador m=
			let dibujador=asociador_texturas_dibujadores#buscar_asociado (m:Geo.textura_bitmap:>Geo.textura) in
			dibujador#destruir;
			asociador_texturas_dibujadores#quitar_asociacion (m:Geo.textura_bitmap:>Geo.textura)
	end;;

class vigilante_no_textura (ag:Geo.no_textura General.agregador_observado)=
	object(self)
		inherit [Geo.no_textura] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_no_textura m in
			asociador_texturas_dibujadores#asociar (m:Geo.no_textura:>Geo.textura) (dibujador:>dibujador_no_textura)
		method eliminar_dibujador m=
			let dibujador=asociador_texturas_dibujadores#buscar_asociado (m:Geo.no_textura:>Geo.textura) in
			dibujador#destruir;
			asociador_texturas_dibujadores#quitar_asociacion (m:Geo.no_textura:>Geo.textura)
	end;;

class virtual dibujador_malla (m:Geo.malla)=
	object(self)
		val malla=m
		method virtual dibujar:unit
		method virtual destruir:unit
	end;;


(* malla_simple *) 
class dibujador_malla_simple_basico (m:Geo.malla_simple)=
	object(self)
		inherit dibujador_malla (m:Geo.malla_simple:>Geo.malla)
		val malla=m
		method dibujar_vertice v=
			let co=v#coordenada in
			let col=v#color in
			let no=v#normal in
			let texcord=List.hd v#uvs in
			let [|x;y;z|]=co#valor#to_array in
			let [|r;g;b;a|]=col#valor#to_array in
			let [|nx;ny;nz|]=no#valor#to_array in
			let [|tx;ty|]=texcord#valor#to_array in
			GlDraw.normal3 (nx, ny, nz);
			GlDraw.color ~alpha:a (r,g,b);
			GlTex.coord2 (tx,ty);
			GlDraw.vertex3 (x, y, z)
			
		method activar_material m=
			let textura=m#textura in
			let dibujador=asociador_texturas_dibujadores#buscar_asociado textura in
			dibujador#activar;

			let ext_color c=
				let [|a;b;c;d|] = c#valor#to_array in
				(a,b,c,d) in
			let ambiente=ext_color m#color_ambiente in
			let difuso=ext_color m#color_difuso in
			let especular=ext_color m#color_especular in
			let brillo=m#brillo /. 100.0 *. 128.0 in
			let emision=ext_color m#color_emision in
			let m_vert=m#modificador_color_vertice in
			let f x=GlLight.material ~face:`front x in
			List.iter f 
			[	`ambient(ambiente);
				`diffuse(difuso);
				`specular(especular);
				`emission(emision);
				`shininess(brillo)
			];
			match m_vert with
			|None -> Gl.disable `color_material
			|Some(a) -> (
				Gl.enable `color_material;
				GlLight.color_material ~face:`front
				(match a with
				|Geo.Ambiente -> `ambient
				|Geo.Difuso -> `diffuse
				|Geo.Especular -> `specular
				|Geo.Emision -> `emission
				|Geo.Ambiente_y_difuso -> `ambient_and_diffuse
				)
			)



		method activar_capa c=
			let material=c#material in
			self#activar_material material



		method dibujar_poligono (p:Geo.poligono_simple)=
			let capa=List.hd p#capas_simples in
			self#activar_capa capa;
			GlDraw.begins `polygon;
			let f v=self#dibujar_vertice v in
			List.iter f p#vertices;
			GlDraw.ends ()
			
		method dibujar=
			let poligonos=malla#poligonos_simples in
			let f (p:Geo.poligono_simple)=self#dibujar_poligono p in
			List.iter f poligonos

		method destruir=()
			
	end;;

(* Malla simple optimizado *)
(* FIXME: Sólo dibuja la primera capa *)
class dibujador_malla_simple_opt (m:Geo.malla_simple)=
	(* Prepara los arrays *)

	(*Hace una lista de todos los vértices de la malla *)
	let vertices=new General.agregador in
	let agrega_todos_vertices ps=
		let agrega_vertices_poligono p=
			let agrega_vertice v=
				vertices#agregar v in
			List.iter agrega_vertice p#vertices in
		List.iter agrega_vertices_poligono ps#poligonos in
	let ()=agrega_todos_vertices m in
	let lista_vertices=vertices#obtener_todos in

	(*asigna un índice a cada vértice*)
	let vertices_indexados=new General.asociador in
	let contador=ref 0 in
	let indexa_vertice v=
		vertices_indexados#asociar v (new General.entero !contador);
		incr contador in
	let ()=List.iter indexa_vertice lista_vertices in

	(*crea arrays para meter los vértices*)
	let array_coords=Array.make ((!contador + 1) * 3) 0.0 in
	let array_norms=Array.make ((!contador + 1) * 3) 0.0 in
	let array_uv1=Array.make ((!contador + 1) * 2) 0.0 in
	let array_color=Array.make ((!contador + 1) * 4) 0.0 in

	(*guarda las coordenadas, normales, uvs y colores en los arrays*)
	let extrae_vertice v=
		let indice=(vertices_indexados#buscar_asociado v)#get in
		let co=v#coordenada#valor#to_array in
		Array.blit co 0 array_coords (indice * 3) 3;
		let no=v#normal#valor#to_array in
		Array.blit no 0 array_norms (indice * 3) 3;
		let col=v#color#valor#to_array in
		Array.blit col 0 array_color (indice * 4) 4;
		let uv=(List.hd v#uvs)#valor#to_array in
		Array.blit uv 0 array_uv1 (indice * 2) 2 in
	let ()=List.iter extrae_vertice lista_vertices in

	(* crea una lista de arrays de índices con los polígonos *)
	let convierte_poligono (p:Geo.poligono)=
		(*busca el índice de un vértice*)
		let convierte_vertice (v:Geo.vertice)= (vertices_indexados#buscar_asociado v)#get in

		(*convierte los vértices del polígono en una lista de índices*)
		let vertices=List.map convierte_vertice (p#vertices) in

		(*convierte una lista de índices en un array*)
		let array_vertices=Array.of_list vertices in

		(*convierte el array de vértices en un raw*)
		let raw_vertices=Raw.of_array array_vertices `uint in

		(*devuelve el resultado: material de la primera capa, raw de
		 * índices de vértices y número de vértices *)
		((List.hd p#capas)#material, raw_vertices, Array.length array_vertices) in

	(*convierte los polígonos en listas de material*raw de indices*num indices*)
	let lista_poligonos=List.map convierte_poligono m#poligonos in

	(*convierte los arrays en raws para opengl*)
	let raw_coords=Raw.of_float_array array_coords `float in
	let raw_norms=Raw.of_float_array array_norms `float in
	let raw_color=Raw.of_float_array array_color `float in
	let raw_uv1=Raw.of_float_array array_uv1 `float in



	object(self)
		inherit dibujador_malla (m:Geo.malla_simple:>Geo.malla)
		val malla=m

		method activar_material m=			
			let textura=m#textura in
			let dibujador=asociador_texturas_dibujadores#buscar_asociado textura in
			dibujador#activar;

			let ext_color c=
				let [|a;b;c;d|] = c#valor#to_array in
				(a,b,c,d) in
			let ambiente=ext_color m#color_ambiente in
			let difuso=ext_color m#color_difuso in
			let especular=ext_color m#color_especular in
			let brillo=m#brillo /. 100.0 *. 128.0 in
			let emision=ext_color m#color_emision in
			let m_vert=m#modificador_color_vertice in
			let f x=GlLight.material ~face:`front x in
			List.iter f 
			[	`ambient(ambiente);
				`diffuse(difuso);
				`specular(especular);
				`emission(emision);
				`shininess(brillo)
			];
			match m_vert with
			|None -> Gl.disable `color_material
			|Some(a) -> (
				Gl.enable `color_material;
				GlLight.color_material ~face:`front
				(match a with
				|Geo.Ambiente -> `ambient
				|Geo.Difuso -> `diffuse
				|Geo.Especular -> `specular
				|Geo.Emision -> `emission
				|Geo.Ambiente_y_difuso -> `ambient_and_diffuse
				)
			)

		val mutable mat_actual=None
		method activar_material_opt m=
			match mat_actual with
			|None-> mat_actual <- Some(m);
					self#activar_material m
			|Some(a) -> 
					if a != m then
						(mat_actual <- Some(m);
						self#activar_material m
						)
					else
						()
						


(*		method activar_capa c=
			let material=c#material in
			self#activar_material_opt material

*)

		method dibujar_poligono (i:Geo.material*[`uint] Raw.t*int)=
			let (m, arr, c)=i in
			self#activar_material_opt m;
			GlArray.draw_elements `polygon c arr
			
		method dibujar=
			mat_actual <- None;
			GlArray.enable `vertex;
			GlArray.vertex `three raw_coords;
			GlArray.enable `normal;
			GlArray.normal raw_norms;
			GlArray.enable `color;
			GlArray.color `four raw_color;
			GlArray.enable `texture_coord;
			GlArray.tex_coord `two raw_uv1;
			let f (mat,arr,c)=self#dibujar_poligono (mat,arr,c) in
			List.iter f lista_poligonos

		method destruir=()
			
	end;;


class dibujador_malla_simple (m:Geo.malla_simple)=
(*	dibujador_malla_simple_basico (m:Geo.malla_simple);;*)
	dibujador_malla_simple_opt (m:Geo.malla_simple);;

let asociador_mallas_dibujadores:(Geo.malla, dibujador_malla) General.asociador=new General.asociador;;

class vigilante_malla_simple (ag:Geo.malla_simple General.agregador_observado)=
	object(self)
		inherit [Geo.malla_simple] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_malla_simple m in
			asociador_mallas_dibujadores#asociar (m:Geo.malla_simple:>Geo.malla) (dibujador:>dibujador_malla)
		method eliminar_dibujador m=
			let dibujador=asociador_mallas_dibujadores#buscar_asociado (m:Geo.malla_simple:>Geo.malla) in
			dibujador#destruir;
			asociador_mallas_dibujadores#quitar_asociacion (m:Geo.malla_simple:>Geo.malla)
	end;;

(******************************************************************************************************)
(* Módulo Objetos                                                                                     *)
(******************************************************************************************************)
class virtual i_dibujador_objeto=
	object(self)
		inherit General.destruible
		method virtual activar_luces:unit
		method virtual dibujar_mallas_p1:unit
		method virtual dibujar_mallas_p2:unit
		method dibujar_malla=()
(*		method virtual dibujar_transparentes:unit*)
		method virtual activar_camara:unit
	end;;

let asociador_objetos_dibujadores:(Objetos.objeto, i_dibujador_objeto) General.asociador=new General.asociador;;

(* Para el ordenador de dibujado de mallas *)
type t_ordenado_mallas=float * i_dibujador_objeto;;

let ordenador_mallas=
	object(self)
		val mutable direccion_camara=Mat.vector3_frontal
		method direccion_camara=direccion_camara
		method set_direccion_camara ndir=
			direccion_camara <- ndir

		val mutable lista_mallas:(t_ordenado_mallas list)=[]
		method borra_lista_mallas=
			lista_mallas <- []

		method add_malla m dist=
			lista_mallas <- (dist,m) :: lista_mallas

		method lista_mallas_ordenada=
			let lista= lista_mallas in
			let ordenador e1 e2=
				let d1,_=e1 in
				let d2,_=e2 in
				Pervasives.compare d1 d2 in
			List.stable_sort ordenador lista
	end;;



(*let direccion_camara=ref Mat.vector3_frontal;;


let lista_mallas = ref ([]:t_ordenado_mallas list);;
let add_malla m dist=
	lista_mallas := (dist,m) :: !lista_mallas;;

let borra_lista_mallas ()=
	lista_mallas := [];;

let lista_mallas_ordenada ()=
	let lista= !lista_mallas in
	let ordenador e1 e2=
		let d1,_=e1 in
		let d2,_=e2 in
		Pervasives.compare d1 d2 in
	List.stable_sort ordenador lista;;
*)
class virtual dibujador_objeto (o:Objetos.objeto)=
	let llamar_dibujadores funcion objetos=
		let llamar_dibujador o=
			let dibujador=asociador_objetos_dibujadores#buscar_asociado o in
			funcion dibujador in
		List.iter llamar_dibujador objetos in
	
	object(self)
		inherit i_dibujador_objeto
		inherit General.observador
		val objeto=o
		
		method actualizar=()
		
		method activar_luz=()

		method añadir_malla=()
		method dibujar_malla=()

(*		method dibujar_transparente=()*)
		method activar_camara=()
		method destruir=()
		method aplicar_transformada=
			let tr=objeto#transformada in
			let [|px;py;pz|]=tr#coordenadas#to_array in
			let v,ang=tr#extrae_rotacion in
			let ang=(ang /. Mat.pi) *. 180.0 in
			let [|rx;ry;rz|]=v#to_array in
			let [|sx;sy;sz|]=tr#dimensiones#to_array in
			GlMat.push ();
			GlMat.translate3 (px,py,pz);
			GlMat.rotate3 ~angle:ang (rx,ry,rz);
			GlMat.scale3 (sx,sy,sz);
			Gl.raise_error "Objeto.aplicar_transformada"
			
		method desaplicar_transformada=
			GlMat.pop ();
			Gl.raise_error "Objeto.desaplicar_transformada"
			
		method activar_luces=
			self#aplicar_transformada;
			Gl.raise_error "Objeto.activar_luces";
			self#activar_luz;
			let hijos_objeto=objeto#hijos in
			llamar_dibujadores (fun x-> x#activar_luces) hijos_objeto;
			self#desaplicar_transformada;
			()


			(* Pasada 1: Se ordenan y cada uno guarda su transformada *)
		method dibujar_mallas_p1=
			self#aplicar_transformada;
			self#añadir_malla;
			let hijos_objeto=objeto#hijos in
			llamar_dibujadores (fun x-> x#dibujar_mallas_p1) hijos_objeto;
			self#desaplicar_transformada

			(* Pasada 2: con la transformada guardada, se dibujan en orden de fondo a frente *)
		method dibujar_mallas_p2=
			let mallas=ordenador_mallas#lista_mallas_ordenada in
			List.iter (fun (_,x)-> x#dibujar_malla) mallas;

(*		method dibujar_transparentes=
			self#aplicar_transformada;
			self#dibujar_transparente;
			let hijos_objeto=objeto#hijos in
			llamar_dibujadores (fun x-> x#dibujar_transparentes) hijos_objeto;
			self#desaplicar_transformada*)
	end;;

let manejador_luces=
	object(self)
		val mutable luz_actual = -1
		method sig_luz=
			luz_actual <- luz_actual + 1;
			match luz_actual with
			| 0-> (`light0, 0)
			| 1-> (`light1, 1)
			| 2-> (`light2, 2)
			| 3-> (`light3, 3)
			| 4-> (`light4, 4)
			| 5-> (`light5, 5)
			| 6-> (`light6, 6)
			| 7-> (`light7, 7)
			| _-> (`light7, 7)
		method reset_luz=
			luz_actual <- -1
	end;;

(*let luz_actual=ref (-1);;
let sig_luz ()=
	incr luz_actual;
	match !luz_actual with
	| 0-> (`light0, 0)
	| 1-> (`light1, 1)
	| 2-> (`light2, 2)
	| 3-> (`light3, 3)
	| 4-> (`light4, 4)
	| 5-> (`light5, 5)
	| 6-> (`light6, 6)
	| 7-> (`light7, 7)
	| _-> (`light7, 7)
;;
let reset_luz ()=
	luz_actual := -1;;*)

(* objeto_origen *)
class dibujador_objeto_origen (o:Objetos.objeto_origen)=
	object(self)
		inherit dibujador_objeto o
		val objeto_origen=o
		method añadir_malla=
			ordenador_mallas#borra_lista_mallas

		method aplicar_transformada=
			()
		method desaplicar_transformada=
			()
		method activar_luz=
			Gl.enable `lighting;
			manejador_luces#reset_luz;
			(*Inicializamos los contadores de luces*)
			let l=[`light0;`light1;`light2;`light3;`light4;`light5;`light6;`light7] in
			List.iter Gl.disable l;
			GlLight.light_model (`ambient (0.0,0.0,0.0,1.0));
			(*luz_actual := Some(`light0,0)*)
	end;;

class vigilante_objeto_origen (ag:Objetos.objeto_origen General.agregador_observado)=
	let upcast m = (m:Objetos.objeto_origen:>Objetos.objeto) in
	object(self)
		inherit [Objetos.objeto_origen] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_objeto_origen m in
			asociador_objetos_dibujadores#asociar (upcast m) (dibujador:>i_dibujador_objeto)
		method eliminar_dibujador m=
			let dibujador=asociador_objetos_dibujadores#buscar_asociado (upcast m) in
			dibujador#destruir;
			asociador_objetos_dibujadores#quitar_asociacion (upcast m)
	end;;

(* dummy *)
class dibujador_dummy (o:Objetos.dummy)=
	object(self)
		inherit dibujador_objeto o
		val dummy=o
	end;;

class vigilante_dummy (ag:Objetos.dummy General.agregador_observado)=
	let upcast m = (m:Objetos.dummy:>Objetos.objeto) in
	object(self)
		inherit [Objetos.dummy] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_dummy m in
			asociador_objetos_dibujadores#asociar (upcast m) (dibujador:>i_dibujador_objeto)
		method eliminar_dibujador m=
			let dibujador=asociador_objetos_dibujadores#buscar_asociado (upcast m) in
			dibujador#destruir;
			asociador_objetos_dibujadores#quitar_asociacion (upcast m)
	end;;



let ext_color c=
let [|a;b;c;d|] = c#valor#to_array in
(a,b,c,d);;

(* punto_luz *) 
class dibujador_punto_luz (o:Objetos.punto_luz)=
	object(self)
		inherit dibujador_objeto (o:>Objetos.objeto)
		val punto_luz=o
		method activar_luz=
			let id_luz, n_luz=manejador_luces#sig_luz in
			let ambiente=ext_color punto_luz#c_ambiente in
			let difuso=ext_color punto_luz#c_difuso in
			let especular=ext_color punto_luz#c_especular in
		
			List.iter (GlLight.light ~num:n_luz)
				[ 
				`ambient ambiente;
				`diffuse difuso;
				`specular especular;
				`constant_attenuation punto_luz#at_constante;
				`linear_attenuation punto_luz#at_lineal;
				`quadratic_attenuation punto_luz#at_cuadratica;
				`position (0.0,0.0,0.0,1.0) ];

				Gl.enable id_luz

	end;;

class vigilante_punto_luz (ag:Objetos.punto_luz General.agregador_observado)=
	let upcast m = (m:Objetos.punto_luz:>Objetos.objeto) in
	object(self)
		inherit [Objetos.punto_luz] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_punto_luz m in
			asociador_objetos_dibujadores#asociar (upcast m) (dibujador:>i_dibujador_objeto)
		method eliminar_dibujador m=
			let dibujador=asociador_objetos_dibujadores#buscar_asociado (upcast m) in
			dibujador#destruir;
			asociador_objetos_dibujadores#quitar_asociacion (upcast m)
	end;;

(* foco *) 
class dibujador_foco (o:Objetos.foco)=
	object(self)
		inherit dibujador_objeto (o:>Objetos.objeto)
		val foco=o
		method activar_luz=
			let id_luz, n_luz=manejador_luces#sig_luz in
			let ambiente=ext_color foco#c_ambiente in
			let difuso=ext_color foco#c_difuso in
			let especular=ext_color foco#c_especular in
			let [|dx;dy;dz|]=Mat.vector3_frontal#to_array in
			let apertura=foco#apertura /. Mat.pi *. 180.0 in
			let exponente=foco#concentracion /. 100.0 *. 128.0 in
		
			List.iter (GlLight.light ~num:n_luz)
				[ 
				`ambient ambiente;
				`diffuse difuso;
				`specular especular;
				`constant_attenuation foco#at_constante;
				`linear_attenuation foco#at_lineal;
				`quadratic_attenuation foco#at_cuadratica;
				`spot_direction (dx,dy,dz);
				`spot_exponent exponente;
				`spot_cutoff apertura;
				`position (0.0,0.0,0.0,1.0) ];

				Gl.enable id_luz
	end;;

class vigilante_foco (ag:Objetos.foco General.agregador_observado)=
	let upcast m = (m:Objetos.foco:>Objetos.objeto) in
	object(self)
		inherit [Objetos.foco] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_foco m in
			asociador_objetos_dibujadores#asociar (upcast m) (dibujador:>i_dibujador_objeto)
		method eliminar_dibujador m=
			let dibujador=asociador_objetos_dibujadores#buscar_asociado (upcast m) in
			dibujador#destruir;
			asociador_objetos_dibujadores#quitar_asociacion (upcast m)
	end;;

(* sol *)
class dibujador_sol (o:Objetos.sol)=
	object(self)
		inherit dibujador_objeto (o:>Objetos.objeto)
		val sol=o
		method activar_luz=
			let id_luz, n_luz=manejador_luces#sig_luz in
			let ext_color c=
				let [|a;b;c;d|] = c#valor#to_array in
				(a,b,c,d) in

			let ambiente=ext_color sol#c_ambiente in
			let difuso=ext_color sol#c_difuso in
			let especular=ext_color sol#c_especular in
			let [|x;y;z|]=Mat.vector3_frontal#to_array in

			List.iter (GlLight.light ~num:n_luz)
				[ 
				`ambient ambiente;
				`diffuse difuso;
				`specular especular;
				`constant_attenuation sol#at_constante;
				`position (x,y,z,0.0) ];

				Gl.enable id_luz;
	end;;

class vigilante_sol (ag:Objetos.sol General.agregador_observado)=
	let upcast m = (m:Objetos.sol:>Objetos.objeto) in
	object(self)
		inherit [Objetos.sol] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_sol m in
			asociador_objetos_dibujadores#asociar (upcast m) (dibujador:>i_dibujador_objeto)
		method eliminar_dibujador m=
			let dibujador=asociador_objetos_dibujadores#buscar_asociado (upcast m) in
			dibujador#destruir;
			asociador_objetos_dibujadores#quitar_asociacion (upcast m)
	end;;

(* camara_perspectiva *) (* FIXME: optimizar *)
let manejador_dimensiones=
	object(self)
		val mutable dim_pantalla=(800,600) (*ancho, alto*)
		method dim_pantalla=dim_pantalla
		method set_dim_pantalla ndim=
			dim_pantalla <- ndim
	end;;

(*let dim_pantalla=ref (800,600);;*)
class dibujador_camara_perspectiva (o:Objetos.camara_perspectiva)=
	object(self)
		inherit dibujador_objeto (o:>Objetos.objeto)
		val camara_perspectiva=o
		method activar_camara=
			GlMat.mode `projection;
			GlMat.load_identity ();
			let pw,ph= manejador_dimensiones#dim_pantalla in
			let pw, ph= float_of_int pw, float_of_int ph in
			let fx,fy=camara_perspectiva#focal in
			let ang_w, ang_h=fx *. pw, fy*. ph in
			let aspecto= ang_w /. ang_h in
			GluMat.perspective ~fovy:(ang_h) ~aspect:(aspecto) ~z:(camara_perspectiva#planos_corte);
			GlMat.mode `modelview;
			GlMat.load_identity();
			let tr=camara_perspectiva#transformada_final in
			let [|px;py;pz|]=tr#coordenadas#to_array in
			let [|dx;dy;dz|]=(tr#coordenadas#suma (tr#orientacion#frontal#producto_escalar (-1.0)))#to_array in
			let [|vx;vy;vz|]=tr#orientacion#vertical#to_array in

			GluMat.look_at ~eye:(px,py,pz) ~center:(dx,dy,dz) ~up:(vx,vy,vz);
			ordenador_mallas#set_direccion_camara tr#orientacion#frontal
			
	end;;

class vigilante_camara_perspectiva (ag:Objetos.camara_perspectiva General.agregador_observado)=
	let upcast m = (m:Objetos.camara_perspectiva:>Objetos.objeto) in
	object(self)
		inherit [Objetos.camara_perspectiva] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_camara_perspectiva m in
			asociador_objetos_dibujadores#asociar (upcast m) (dibujador:>i_dibujador_objeto)
		method eliminar_dibujador m=
			let dibujador=asociador_objetos_dibujadores#buscar_asociado (upcast m) in
			dibujador#destruir;
			asociador_objetos_dibujadores#quitar_asociacion (upcast m)
	end;;

(* camara_ortografica *) (* FIXME: Optimizar *)
class dibujador_camara_ortografica (o:Objetos.camara_ortografica)=
	object(self)
		inherit dibujador_objeto (o:>Objetos.objeto)
		val camara_ortografica=o
		method activar_camara=
			GlMat.mode `projection;
			GlMat.load_identity ();
			let ancho,alto=camara_ortografica#volumen in
			let x1,x2 = -.ancho /. 2.0, ancho /. 2.0 in
			let y1,y2 = -.alto /. 2.0, alto /. 2.0 in
			let z1,z2 = camara_ortografica#planos_corte in
			GlMat.ortho ~x:(x1,x2) ~y:(y1,y2) ~z:(z1, z2);

			GlMat.mode `modelview;
			GlMat.load_identity();
			let tr=camara_ortografica#transformada_final in
			let [|px;py;pz|]=tr#coordenadas#to_array in
			let [|dx;dy;dz|]=(tr#coordenadas#suma (tr#orientacion#frontal#producto_escalar (-1.0)))#to_array in
			let [|vx;vy;vz|]=tr#orientacion#vertical#to_array in

			GluMat.look_at ~eye:(px,py,pz) ~center:(dx,dy,dz) ~up:(vx,vy,vz);
			ordenador_mallas#set_direccion_camara tr#orientacion#frontal

	end;;

class vigilante_camara_ortografica (ag:Objetos.camara_ortografica General.agregador_observado)=
	let upcast m = (m:Objetos.camara_ortografica:>Objetos.objeto) in
	object(self)
		inherit [Objetos.camara_ortografica] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_camara_ortografica m in
			asociador_objetos_dibujadores#asociar (upcast m) (dibujador:>i_dibujador_objeto)
		method eliminar_dibujador m=
			let dibujador=asociador_objetos_dibujadores#buscar_asociado (upcast m) in
			dibujador#destruir;
			asociador_objetos_dibujadores#quitar_asociacion (upcast m)
	end;;

(* objeto_malla *)
class dibujador_objeto_malla (o:Objetos.objeto_malla)=
	object(self)
		inherit dibujador_objeto (o:>Objetos.objeto)
		val objeto_malla=o
		val mutable mi_transformada=GlMat.get_matrix `modelview_matrix
		method añadir_malla=
			mi_transformada <- GlMat.get_matrix `modelview_matrix;
			let matriz=GlMat.to_array mi_transformada in
			let px=matriz.(3).(0) in
			let py=matriz.(3).(1) in
			let pz=matriz.(3).(2) in
			let pos=new Mat.vector3 [|px;py;pz|] in
			let dist=pos#producto_interior (ordenador_mallas#direccion_camara) in
			ordenador_mallas#add_malla (self:>i_dibujador_objeto) dist

		method dibujar_malla=
			GlMat.load mi_transformada;
			let dibujador_malla= asociador_mallas_dibujadores#buscar_asociado (o#malla) in
			dibujador_malla#dibujar
(*		method dibujar_transparente=()*)
	end;;

class vigilante_objeto_malla (ag:Objetos.objeto_malla General.agregador_observado)=
	let upcast m = (m:Objetos.objeto_malla:>Objetos.objeto) in
	object(self)
		inherit [Objetos.objeto_malla] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_objeto_malla m in
			asociador_objetos_dibujadores#asociar (upcast m) (dibujador:>i_dibujador_objeto)
		method eliminar_dibujador m=
			let dibujador=asociador_objetos_dibujadores#buscar_asociado (upcast m) in
			dibujador#destruir;
			asociador_objetos_dibujadores#quitar_asociacion (upcast m)
	end;;

(******************************************************************************************************)
(* Módulo Render                                                                                      *)
(******************************************************************************************************)
class virtual dibujador_capa (c:Render.capa)=
	object(self)
		inherit General.destruible
		val capa=c
		method virtual dibujar:unit
		method destruir=()
	end;;

let asociador_capas_dibujadores:(Render.capa, dibujador_capa) General.asociador=new General.asociador;;

(* capa3d *)
class dibujador_capa3d (c:Render.capa3d)=
	object(self)
		inherit dibujador_capa (c:>Render.capa)
		val capa3d=c
		method dibujar=
			let dibujador_camara=asociador_objetos_dibujadores#buscar_asociado (capa3d#camara:>Objetos.objeto) in
			dibujador_camara#activar_camara;
			let dibujador_objeto_origen=asociador_objetos_dibujadores#buscar_asociado (capa3d#objeto_origen) in
			dibujador_objeto_origen#activar_luces;
			dibujador_objeto_origen#dibujar_mallas_p1;
			dibujador_objeto_origen#dibujar_mallas_p2;
(*			dibujador_objeto_origen#dibujar_transparentes*)
	end;;

class vigilante_capa3d (ag:Render.capa3d General.agregador_observado)=
	let upcast m = (m:Render.capa3d:>Render.capa) in
	object(self)
		inherit [Render.capa3d] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_capa3d m in
			asociador_capas_dibujadores#asociar (upcast m) (dibujador:>dibujador_capa)
		method eliminar_dibujador m=
			let dibujador=asociador_capas_dibujadores#buscar_asociado (upcast m) in
			dibujador#destruir;
			asociador_capas_dibujadores#quitar_asociacion (upcast m)
	end;;

(* capa_nula *)
class dibujador_capa_nula (c:Render.capa_nula)=
	object(self)
		inherit dibujador_capa (c:>Render.capa)
		val capa_nula=c
		method dibujar=()
	end;;

class vigilante_capa_nula (ag:Render.capa_nula General.agregador_observado)=
	let upcast m = (m:Render.capa_nula:>Render.capa) in
	object(self)
		inherit [Render.capa_nula] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_capa_nula m in
			asociador_capas_dibujadores#asociar (upcast m) (dibujador:>dibujador_capa)
		method eliminar_dibujador m=
			let dibujador=asociador_capas_dibujadores#buscar_asociado (upcast m) in
			dibujador#destruir;
			asociador_capas_dibujadores#quitar_asociacion (upcast m)
	end;;

(* capa_borrado *)
class dibujador_capa_borrado (c:Render.capa_borrado)=
	object(self)
		inherit dibujador_capa (c:>Render.capa)
		val capa_borrado=c
		method dibujar=
			let [|r;g;b;a|]=c#color#valor#to_array in
			GlClear.color ~alpha:a (r,g,b);
			GlClear.clear [`color]
	end;;

class vigilante_capa_borrado (ag:Render.capa_borrado General.agregador_observado)=
	let upcast m = (m:Render.capa_borrado:>Render.capa) in
	object(self)
		inherit [Render.capa_borrado] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_capa_borrado m in
			asociador_capas_dibujadores#asociar (upcast m) (dibujador:>dibujador_capa)
		method eliminar_dibujador m=
			let dibujador=asociador_capas_dibujadores#buscar_asociado (upcast m) in
			dibujador#destruir;
			asociador_capas_dibujadores#quitar_asociacion (upcast m)
	end;;

(* capa_borrado_zbuffer *)
class dibujador_capa_borrado_zbuffer (c:Render.capa_borrado_zbuffer)=
	object(self)
		inherit dibujador_capa (c:>Render.capa)
		val capa_borrado=c
		method dibujar=
			GlClear.clear [`depth]
	end;;
			
class vigilante_capa_borrado_zbuffer (ag:Render.capa_borrado_zbuffer General.agregador_observado)=
	let upcast m = (m:Render.capa_borrado_zbuffer:>Render.capa) in
	object(self)
		inherit [Render.capa_borrado_zbuffer] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_capa_borrado_zbuffer m in
			asociador_capas_dibujadores#asociar (upcast m) (dibujador:>dibujador_capa)
		method eliminar_dibujador m=
			let dibujador=asociador_capas_dibujadores#buscar_asociado (upcast m) in
			dibujador#destruir;
			asociador_capas_dibujadores#quitar_asociacion (upcast m)
	end;;

(* escena *)
class dibujador_escena (e:Render.escena)=
	object(self)
		inherit General.destruible
		method destruir=()
		val escena=e
		method dibujar=
			let f x=
				let dibujador=asociador_capas_dibujadores#buscar_asociado x in
				dibujador#dibujar in
			Array.iter f escena#capas
	end;;

let asociador_escenas_dibujadores:(Render.escena, dibujador_escena) General.asociador=new General.asociador;;

class vigilante_escena (ag:Render.escena General.agregador_observado)=
	object(self)
		inherit [Render.escena] vigilante_agregador ag
		method nuevo_dibujador m=
			let dibujador=new dibujador_escena m in
			asociador_escenas_dibujadores#asociar m dibujador
		method eliminar_dibujador m=
			let dibujador=asociador_escenas_dibujadores#buscar_asociado m in
			dibujador#destruir;
			asociador_escenas_dibujadores#quitar_asociacion m
	end;;


class virtual dibujador_pantalla (p:Render.pantalla)=
	object(self)
		inherit General.observador
		val pantalla=p
		method dibujar=
			let dimx, dimy=pantalla#dimensiones in
			manejador_dimensiones#set_dim_pantalla (dimx, dimy);
			match pantalla#escena with
			|Some(escena) ->			
				let dibujador=asociador_escenas_dibujadores#buscar_asociado (escena) in
				dibujador#dibujar
			|None -> ()
	end;;

type tmodopantalla=Desconocido | PantallaCompleta | Ventana;;
(*let modo_actual=ref Desconocido;;*)



class dibujador_monitor (m:Render.monitor)=
	object(self)
		inherit dibujador_pantalla (m:>Render.pantalla)
		val monitor=m
		val mutable modo_anterior=m#estado
		method actualizar=
			let dx, dy=monitor#dimensiones in
			(match modo_anterior, monitor#estado with
			| _, Render.Pantalla_completa ->
				ignore (Sdlvideo.set_video_mode ~w:dx ~h:dy [`OPENGL;`FULLSCREEN])
			| _, Render.Ventana ->
				ignore (Sdlvideo.set_video_mode ~w:dx ~h:dy [`OPENGL])
			| _-> ());
			GlDraw.viewport ~x:0 ~y:0 ~w:dx ~h:dy;
			GlMat.mode `projection;
			GlMat.load_identity (); 
			GlMat.mode `modelview; 
			GlMat.load_identity();
			GlClear.color (0.0,0.0,0.0);
  			Gl.enable `depth_test;
			GlDraw.cull_face `back;
			Gl.enable `cull_face;
			Gl.enable `blend;
			GlFunc.blend_func `src_alpha `one_minus_src_alpha;

			(* Re-mandamos todas las texturas a opengl *)

			let bitmaps=Bitmaps.contador_bitmap_RGBA#obtener_todos in
			let f_busca_dibujador x=asociador_bitmaps_dibujadores#buscar_asociado (x:>Geo.bitmap) in
			let dibujadores_bitmaps=List.map f_busca_dibujador bitmaps in
			List.iter (fun x-> x#destruir) dibujadores_bitmaps;
			List.iter (fun x-> x#activar) dibujadores_bitmaps

			
	end;;

let dibujadores_pantalla:dibujador_pantalla General.agregador=new General.agregador;;

(* motor_grafico *)
class motor_grafico_opengl=  
	object(self)
		inherit Vista.motor

		val mutable vigilante_bitmap_RGBA=None
		method preparar_dibujadores_bitmap_RGBA=
			let v=new vigilante_bitmap_RGBA Bitmaps.contador_bitmap_RGBA in
			vigilante_bitmap_RGBA <- Some(v);
			let bitmaps=Bitmaps.contador_bitmap_RGBA#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) bitmaps

		val mutable vigilante_textura_bitmap=None
		method preparar_dibujadores_textura_bitmap=
			let v=new vigilante_textura_bitmap Geo.contador_textura_bitmap in
			vigilante_textura_bitmap <- Some(v);
			let texturas=Geo.contador_textura_bitmap#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) texturas

		val mutable vigilante_no_textura=None
		method preparar_dibujadores_no_textura=
			let v=new vigilante_no_textura Geo.contador_no_textura in
			vigilante_no_textura <- Some(v);
			let texturas=Geo.contador_no_textura#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) texturas
		
		val mutable vigilante_malla_simple=None
		method preparar_dibujadores_malla_simple=
			let v=new vigilante_malla_simple Geo.contador_malla_simple in
			vigilante_malla_simple <- Some(v);
			let mallas=Geo.contador_malla_simple#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) mallas
			
		val mutable vigilante_objeto_origen=None
		method preparar_dibujadores_objeto_origen=
			let v=new vigilante_objeto_origen Objetos.contador_objeto_origen in
			vigilante_objeto_origen <- Some(v);
			let objetos=Objetos.contador_objeto_origen#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) objetos
			
		val mutable vigilante_dummy=None
		method preparar_dibujadores_dummy=
			let v=new vigilante_dummy Objetos.contador_dummy in
			vigilante_dummy <- Some(v);
			let objetos=Objetos.contador_dummy#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) objetos

		val mutable vigilante_punto_luz=None
		method preparar_dibujadores_punto_luz=
			let v=new vigilante_punto_luz Objetos.contador_punto_luz in
			vigilante_punto_luz <- Some(v);
			let objetos=Objetos.contador_punto_luz#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) objetos

		val mutable vigilante_foco=None
		method preparar_dibujadores_foco=
			let v=new vigilante_foco Objetos.contador_foco in
			vigilante_foco <- Some(v);
			let objetos=Objetos.contador_foco#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) objetos

		val mutable vigilante_sol=None
		method preparar_dibujadores_sol=
			let v=new vigilante_sol Objetos.contador_sol in
			vigilante_sol <- Some(v);
			let objetos=Objetos.contador_sol#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) objetos

		val mutable vigilante_camara_perspectiva=None
		method preparar_dibujadores_camara_perspectiva=
			let v=new vigilante_camara_perspectiva Objetos.contador_camara_perspectiva in
			vigilante_camara_perspectiva <- Some(v);
			let objetos=Objetos.contador_camara_perspectiva#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) objetos

		val mutable vigilante_camara_ortografica=None
		method preparar_dibujadores_camara_ortografica=
			let v=new vigilante_camara_ortografica Objetos.contador_camara_ortografica in
			vigilante_camara_ortografica <- Some(v);
			let objetos=Objetos.contador_camara_ortografica#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) objetos

		val mutable vigilante_objeto_malla=None
		method preparar_dibujadores_objeto_malla=
			let v=new vigilante_objeto_malla Objetos.contador_objeto_malla in
			vigilante_objeto_malla <- Some(v);
			let objetos=Objetos.contador_objeto_malla#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) objetos

		val mutable vigilante_capa3d=None
		method preparar_dibujadores_capa3d=
			let v=new vigilante_capa3d Render.contador_capa3d in
			vigilante_capa3d <- Some(v);
			let objetos=Render.contador_capa3d#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) objetos

		val mutable vigilante_capa_nula=None
		method preparar_dibujadores_capa_nula=
			let v=new vigilante_capa_nula Render.contador_capa_nula in
			vigilante_capa_nula <- Some(v);
			let objetos=Render.contador_capa_nula#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) objetos

		val mutable vigilante_capa_borrado=None
		method preparar_dibujadores_capa_borrado=
			let v=new vigilante_capa_borrado Render.contador_capa_borrado in
			vigilante_capa_borrado <- Some(v);
			let objetos=Render.contador_capa_borrado#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) objetos

		val mutable vigilante_capa_borrado_zbuffer=None
		method preparar_dibujadores_capa_borrado_zbuffer=
			let v=new vigilante_capa_borrado_zbuffer Render.contador_capa_borrado_zbuffer in
			vigilante_capa_borrado_zbuffer <- Some(v);
			let objetos=Render.contador_capa_borrado_zbuffer#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) objetos

		val mutable vigilante_escena=None
		method preparar_dibujadores_escena=
			let v=new vigilante_escena Render.contador_escena in
			vigilante_escena <- Some(v);
			let objetos=Render.contador_escena#obtener_todos in
			List.iter (fun x->v#nuevo_dibujador x) objetos

		
		method inicializar=
			let d_pantalla=new dibujador_monitor Render.monitor_principal in
			dibujadores_pantalla#agregar (d_pantalla:>dibujador_pantalla);
			Render.monitor_principal#añadir_observador (d_pantalla:>General.observador);

(*			Render.monitor_principal#cambiar_modo 800 600 Render.Ventana;*)
			Render.monitor_principal#notificar;
			

			
			self#preparar_dibujadores_bitmap_RGBA;

			self#preparar_dibujadores_textura_bitmap;
			self#preparar_dibujadores_no_textura;

			self#preparar_dibujadores_malla_simple;
			
			self#preparar_dibujadores_objeto_origen;
			self#preparar_dibujadores_dummy;
			self#preparar_dibujadores_punto_luz;
			self#preparar_dibujadores_foco;
			self#preparar_dibujadores_sol;
			self#preparar_dibujadores_camara_perspectiva;
			self#preparar_dibujadores_camara_ortografica;
			self#preparar_dibujadores_objeto_malla;
			
			self#preparar_dibujadores_capa3d;
			self#preparar_dibujadores_capa_nula;
			self#preparar_dibujadores_capa_borrado;
			self#preparar_dibujadores_capa_borrado_zbuffer;
			self#preparar_dibujadores_escena

			
		method dibujarFotograma=
			let dib=dibujadores_pantalla#obtener_todos in
			GlMat.mode `modelview;
			let f x=x#dibujar in
			List.iter f dib;
			Sdlgl.swap_buffers ()

		method destruir_dibujadores_bitmap_RGBA=
			(match vigilante_bitmap_RGBA with
				| Some(v) ->
					let bitmaps=Bitmaps.contador_bitmap_RGBA#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) bitmaps;
					v#destruir
				| _ -> ());
			vigilante_bitmap_RGBA <- None

		method destruir_dibujadores_textura_bitmap=
			(match vigilante_textura_bitmap with
				| Some(v) ->
					let items=Geo.contador_textura_bitmap#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_textura_bitmap <- None

		method destruir_dibujadores_no_textura=
			(match vigilante_no_textura with
				| Some(v) ->
					let items=Geo.contador_no_textura#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_no_textura <- None
			
		method destruir_dibujadores_malla_simple=
			(match vigilante_malla_simple with
				| Some(v) ->
					let items=Geo.contador_malla_simple#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_malla_simple <- None

		method destruir_dibujadores_objeto_origen=
			(match vigilante_objeto_origen with
				| Some(v) ->
					let items=Objetos.contador_objeto_origen#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_objeto_origen <- None

		method destruir_dibujadores_dummy=
			(match vigilante_dummy with
				| Some(v) ->
					let items=Objetos.contador_dummy#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_dummy <- None

		method destruir_dibujadores_punto_luz=
			(match vigilante_punto_luz with
				| Some(v) ->
					let items=Objetos.contador_punto_luz#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_punto_luz <- None

		method destruir_dibujadores_foco=
			(match vigilante_foco with
				| Some(v) ->
					let items=Objetos.contador_foco#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_foco <- None

		method destruir_dibujadores_sol=
			(match vigilante_sol with
				| Some(v) ->
					let items=Objetos.contador_sol#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_sol <- None			

		method destruir_dibujadores_camara_perspectiva=
			(match vigilante_camara_perspectiva with
				| Some(v) ->
					let items=Objetos.contador_camara_perspectiva#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_camara_perspectiva <- None	

		method destruir_dibujadores_camara_ortografica=
			(match vigilante_camara_ortografica with
				| Some(v) ->
					let items=Objetos.contador_camara_ortografica#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_camara_ortografica <- None	

		method destruir_dibujadores_objeto_malla=
			(match vigilante_objeto_malla with
				| Some(v) ->
					let items=Objetos.contador_objeto_malla#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_objeto_malla <- None	

		method destruir_dibujadores_capa3d=
			(match vigilante_capa3d with
				| Some(v) ->
					let items=Render.contador_capa3d#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_capa3d <- None	

		method destruir_dibujadores_capa_nula=
			(match vigilante_capa_nula with
				| Some(v) ->
					let items=Render.contador_capa_nula#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_capa_nula <- None	
			
		method destruir_dibujadores_capa_borrado=
			(match vigilante_capa_borrado with
				| Some(v) ->
					let items=Render.contador_capa_borrado#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_capa_borrado <- None	

		method destruir_dibujadores_capa_borrado_zbuffer=
			(match vigilante_capa_borrado_zbuffer with
				| Some(v) ->
					let items=Render.contador_capa_borrado_zbuffer#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_capa_borrado_zbuffer <- None	

		method destruir_dibujadores_escena=
			(match vigilante_escena with
				| Some(v) ->
					let items=Render.contador_escena#obtener_todos in
					List.iter (fun x->v#eliminar_dibujador x) items;
					v#destruir
				| _ -> ());
			vigilante_escena <- None	
			
			

		method terminar= 
			self#destruir_dibujadores_bitmap_RGBA;

			self#destruir_dibujadores_textura_bitmap;
			self#destruir_dibujadores_no_textura;

			self#destruir_dibujadores_malla_simple;
			
			self#destruir_dibujadores_objeto_origen;
			self#destruir_dibujadores_dummy;
			self#destruir_dibujadores_punto_luz;
			self#destruir_dibujadores_foco;
			self#destruir_dibujadores_sol;
			self#destruir_dibujadores_camara_perspectiva;
			self#destruir_dibujadores_camara_ortografica;
			self#destruir_dibujadores_objeto_malla;
			
			self#destruir_dibujadores_capa3d;
			self#destruir_dibujadores_capa_nula;
			self#destruir_dibujadores_capa_borrado;
			self#destruir_dibujadores_capa_borrado_zbuffer;
			self#destruir_dibujadores_escena
			

	end;;

let motor_grafico_opengl=new motor_grafico_opengl;;

Vista.motores_graficos#registra_motor "OpenGL" (motor_grafico_opengl:>Vista.motor);;
			
