class posicion=
	object(self)
		val mutable coordenadas=new Mat.vector [|0.0;0.0;0.0|]
		method coordenadas=coordenadas
		method mover (d:Mat.vector)=
			coordenadas <- coordenadas#suma d
		method establecer_coordenadas (c:Mat.vector)=
			if c#num_elementos = 3 then
				coordenadas <- c
			else
				failwith "posicion#establecer_coordenadas v: v no es un vector de dimensión 3"
	end;;

class orientacion=
	object(self)
		val mutable cuaternio=Mat.cuaternio_rotacion (new Mat.vector [|1.0;0.0;0.0|]) 0.0
		method cuaternio=cuaternio
		method extrae_rotacion:(Mat.vector * float)=
			cuaternio#extrae_rotacion
		method rotar (v:Mat.vector) ang=
			let cuat2=Mat.cuaternio_rotacion v ang in
			(*cuaternio <- (cuaternio#producto cuat2)#a_unitario*)
			(*cuaternio <- (cuat2#producto cuaternio)#a_unitario*)
			cuaternio <- cuat2#producto cuaternio
		method establecer_rotacion (v:Mat.vector) ang=
			cuaternio <- Mat.cuaternio_rotacion v ang
		method rota_vector v=
			let ccuat=cuaternio#conjugado in
			let v=Mat.cuaternio_vector v in
			let cuat_res=(cuaternio#producto v)#producto ccuat in
			cuat_res#extrae_vector
		method frontal=
			self#rota_vector (new Mat.vector [|0.0;0.0;1.0|])
		method vertical=
			self#rota_vector (new Mat.vector [|0.0;1.0;0.0|])
	end;;

class escala=
	object(self)
		val mutable escala=new Mat.vector [|1.0;1.0;1.0|]
		method escala=escala
		method escalar (d:Mat.vector)=
			let ex,ey,ez = 
				match escala#to_array with
				[|x;y;z|] -> x,y,z 
				|_-> failwith "escala#escalar: Esto no puede ocurrir" in
			
			let nx,ny,nz=
				match d#to_array with
				[|a;b;c|] -> a,b,c 
				|_->failwith "escala#escalar d: d no es un vector de dimension 3" in
			
			escala <- new Mat.vector [|ex *. nx ; ey *. ny ; ez *. nz|]
		method establecer_escala (c:Mat.vector)=
			if c#num_elementos = 3 then
				escala <- c
			else
				failwith "escala#establecer_escala v: v no es un vector de dimensión 3"
	end;;

class transformada=
	object(self)
		val posicion=new posicion
		method posicion=posicion
		val orientacion=new orientacion
		method orientacion=orientacion
		val escala=new escala
		method escala=escala
		
		method mover v=
			posicion#mover v
		method establecer_posicion v=
			posicion#establecer_coordenadas v
		method coordenadas=
			posicion#coordenadas

		method escalar v=
			escala#escalar v
		method establecer_escala v=
			escala#establecer_escala v
		method dimensiones=
			escala#escala

		method rotar v ang=
			orientacion#rotar v ang
		method establecer_rotacion v ang=
			orientacion#establecer_rotacion v ang
		method extrae_rotacion=
			orientacion#extrae_rotacion

		method a_matriz=
			let [|co_x;co_y;co_z|]=self#coordenadas#to_array in
			let [|sx;sy;sz|]=self#escala#escala#to_array in
			let mr=self#orientacion#cuaternio#a_matriz_rotacion in
			let mt=Mat.matriz_translacion co_x co_y co_z in
			let ms=Mat.matriz_escala sx sy sz in
			(*Registro.mensaje ("Translacion: \n" ^ mt#a_string);
			Registro.mensaje ("Rotacion: \n" ^ mr#a_string);
			Registro.mensaje ("Escala: \n" ^ ms#a_string);*)

			let res=(mt#matriz_por_matriz mr)#matriz_por_matriz ms in
			(*Registro.mensaje ("Transformada: \n" ^ res#a_string);*)
			res

	end;;

let extrae_transformada (mi:Mat.matriz)=
	(*Registro.mensaje ("Matriz para extrae_transformada: \n" ^ mi#a_string);*)
	(* Lento e ineficiente, y algo incompleto *)
	let m=mi#duplica in

	(* Vector translación *)
	let v_translacion=m#extrae_columna 4 in
	let [|tx;ty;tz;_|]=v_translacion#to_array in
	
	(*m#cambia_elemento 1 4 0.0;
	m#cambia_elemento 2 4 0.0;
	m#cambia_elemento 3 4 0.0;*)

	(*Registro.mensaje ("Matriz sin translacion: \n" ^ m#a_string);*)

	(* Escala *)
	let vx=m#extrae_columna 1 in
	let vy=m#extrae_columna 2 in
	let vz=m#extrae_columna 3 in
	
	let sx=vx#longitud in
	let sy=vy#longitud in
	let sz=vz#longitud in

	let antiescala=Mat.matriz_escala (1.0 /. sx) (1.0 /. sy) (1.0 /. sz) in
	let m=antiescala#matriz_por_matriz m in
	
	(* Rotación *)
	let matriz=m#extrae_submatriz (1,3) (1,3) in
	(*Registro.mensaje ("Matriz para calcular rotacion: \n" ^ matriz#a_string);*)

	let m i j=matriz#extrae_elemento i j in
	let traza=(m 1 1) +. (m 2 2) +. (m 3 3) +. 1.0 in
	let cuaternion=
		if traza > 0.0 then
			let s= 0.5 /. (sqrt traza) in
			let w= 0.25 /. s in
			let x= ((m 3 2) -. (m 2 3)) *. s in
			let y= ((m 1 3) -. (m 3 1)) *. s in
			let z= ((m 2 1) -. (m 1 2)) *. s in
			new Mat.cuaternion w (x,y,z)
		else
			let (w,x,y,z)=match (m 1 1),(m 2 2),(m 3 3) with
			|(a,b,c) when (a > b) && (a > c) ->
				let s= (sqrt (1.0 +. (m 1 1) -. (m 2 2) -. (m 3 3))) *. 2.0 in
				let x= 0.5 /. s in
				let y= ((m 1 2) -. (m 2 1)) *. s in
				let z= ((m 1 3) -. (m 3 1)) *. s in
				let w= ((m 2 3) -. (m 3 2)) *. s in
				(w,x,y,z)
			
			|(a,b,c) when (b > a) && (b > c) ->
				let s= (sqrt (1.0 +. (m 2 2) -. (m 1 1) -. (m 3 3))) *. 2.0 in
				let x= ((m 1 2) -. (m 2 1)) *. s in
				let y= 0.5 /. s in
				let z= ((m 2 3) -. (m 3 2)) *. s in
				let w= ((m 1 3) -. (m 3 1)) *. s in
				(w,x,y,z)
			
			|(a,b,c) ->
				let s= (sqrt (1.0 +. (m 3 3) -. (m 1 1) -. (m 2 2))) *. 2.0 in
				let x= ((m 1 3) -. (m 3 1)) *. s in
				let y= ((m 2 3) -. (m 3 2)) *. s in
				let z= 0.5 /. s in
				let w= ((m 1 2) -. (m 2 1)) *. s in
				(w,x,y,z)
			in
			new Mat.cuaternion w (x,y,z) in
	let v,ang=cuaternion#extrae_rotacion in

	(* nueva transformada *)
	let trans=new transformada in
	trans#establecer_posicion (new Mat.vector3 [|tx;ty;tz|]);
	trans#establecer_rotacion v ang;
	trans#establecer_escala (new Mat.vector3 [|sx;sy;sz|]);
	(*Registro.mensaje ("Transformada final: \n" ^ trans#a_matriz#a_string);*)

	trans;;
	
	
	

class virtual objeto=
	object(self)
		inherit General.sujeto
		val mutable transformada=new transformada
		method transformada=transformada
		method set_transformada t=
			transformada <- t
		val mutable padre=(None:objeto option)
		method padre=
			match padre with
			|None -> failwith "El objeto no tiene padre"
			|Some(a) -> a

		method establecer_padre (x:objeto)=
			padre <- Some(x);
			self#notificar

		method hacer_huerfano =
			(try
				self#padre#quitar_hijo (self:>objeto)
			with
				Failure "El objeto no tiene padre"-> ());
			padre <- None;
			self#notificar

		val mutable hijos=([]:objeto list)
		method hijos=hijos
		method añadir_hijo (x:objeto)=
			(if not (List.memq x hijos) then
				hijos <- x :: hijos);
			self#notificar
		method quitar_hijo (x:objeto)=
			let f a= a<>x in
			hijos <- List.filter f hijos;
			self#notificar
		method emparentar (p:objeto)=
			(try
				(self#padre)#quitar_hijo (self:>objeto)
			with
				Failure("El objeto no tiene padre")-> ());
			p#añadir_hijo (self:>objeto);
			self#establecer_padre p

		method origen:objeto=
			self#padre#origen

		method matriz_transformada_final=
			let mi_m=self#transformada#a_matriz in
			let padre_m=self#padre#matriz_transformada_final in
			let res=padre_m#matriz_por_matriz mi_m in
			(*Registro.mensaje ("Matriz transformada final: \n" ^ res#a_string);*)
			res

			
		method transformada_final=
			let mi_m=self#matriz_transformada_final in
			extrae_transformada mi_m			
	end;;

class c_objeto_origen=
	object(self)
		inherit objeto
		method padre=
			failwith "objeto_origen#padre : Los objetos origen no tienen padre"
		method establecer_padre=
			failwith "objeto_origen#establecer_padre : Los objetos origen no tienen padre"
		method origen=
			(self:>objeto)

		method matriz_transformada_final=
			Mat.matriz_identidad 4
	end;;

let contador_objeto_origen:c_objeto_origen General.agregador_observado=new General.agregador_observado;;
class objeto_origen=
	object(self)
		inherit c_objeto_origen
		initializer contador_objeto_origen#agregar (self:>c_objeto_origen)
	end;;


class c_dummy=
	object(self)
		inherit objeto
	end;;

let contador_dummy:c_dummy General.agregador_observado=new General.agregador_observado;;
class dummy=
	object(self)
		inherit c_dummy
		initializer contador_dummy#agregar (self:>c_dummy)
	end;;

let color r g b a=
	new Geo.color_simple (new Mat.vector4 [|r;g;b;a|]);;

class virtual luz=
	object(self)
		inherit objeto
		val mutable c_ambiente = color 0.0 0.0 0.0 1.0
		method c_ambiente=c_ambiente
		method set_c_ambiente c=
			c_ambiente <- c

		val mutable c_difuso = color 1.0 1.0 1.0 1.0
		method c_difuso=c_difuso
		method set_c_difuso c=
			c_difuso <- c

		val mutable c_especular = color 1.0 1.0 1.0 1.0
		method c_especular=c_especular
		method set_c_especular c=
			c_especular <- c

		val mutable at_constante = 1.0
		method at_constante=at_constante
		method set_at_constante a=
			at_constante <- a
			
	end;;

class virtual luz_puntual=
	object(self)
		inherit luz
		val mutable at_lineal = 0.0
		method at_lineal=at_lineal
		method set_at_lineal a=
			at_lineal <- a

		val mutable at_cuadratica = 0.0
		method at_cuadratica=at_cuadratica
		method set_at_cuadratica a=
			at_cuadratica <- a
	end;;

class c_punto_luz=
	object(self)
		inherit luz_puntual
	end;;

let contador_punto_luz : c_punto_luz General.agregador_observado=new General.agregador_observado;;
class punto_luz=
	object(self)
		inherit c_punto_luz
		initializer contador_punto_luz#agregar (self:>c_punto_luz)
	end;;


class c_foco=
	object(self)
		inherit luz_puntual
		val mutable apertura = Mat.pi /. 4.0
		method apertura=apertura
		method set_apertura a=
			apertura <- a

		val mutable concentracion = 0.0
		method concentracion=concentracion
		method set_concentracion a=
			concentracion <- a
	end;;

let contador_foco:c_foco General.agregador_observado=new General.agregador_observado;;
class foco=
	object(self)
		inherit c_foco
		initializer contador_foco#agregar (self:>c_foco)
	end;;


class c_sol=
	object(self)
		inherit luz
	end;;

let contador_sol:c_sol General.agregador_observado=new General.agregador_observado;;
class sol=
	object(self)
		inherit c_sol
		initializer contador_sol#agregar (self:>c_sol)
	end;;

class virtual camara=
	object(self)
		inherit objeto
		val mutable plano_cercano = 0.5
		val mutable plano_lejano = 100.0
		method planos_corte=(plano_cercano, plano_lejano)
		method set_planos_corte (a,b)=
			plano_cercano <- a;
			plano_lejano <- b
	end;;

class c_camara_perspectiva=
	object(self)
		inherit camara
		val mutable focal_x=0.075
		val mutable focal_y=0.075
		method focal=(focal_x,focal_y)
		method set_focal (x,y)=
			focal_x <- x;
			focal_y <- y
	end;;

let contador_camara_perspectiva:c_camara_perspectiva General.agregador_observado=new General.agregador_observado;;
class camara_perspectiva=
	object(self)
		inherit c_camara_perspectiva
		initializer contador_camara_perspectiva#agregar (self:>c_camara_perspectiva)
	end;;


class c_camara_ortografica=
	object(self)
		inherit camara
		val mutable ancho=8.0
		val mutable alto=6.0
		method volumen=(ancho,alto)
		method set_volumen (x,y)=
			ancho <- x;
			alto <- y
	end;;

let contador_camara_ortografica:c_camara_ortografica General.agregador_observado=new General.agregador_observado;;
class camara_ortografica=
	object(self)
		inherit c_camara_ortografica
		initializer contador_camara_ortografica#agregar (self:>c_camara_ortografica)
	end;;

class c_objeto_malla=
	object(self)
		inherit objeto
		val mutable malla=None
		method malla=
			match malla with
			|Some(a) -> a
			|None -> failwith "objeto_malla#malla : no hay malla asociada"			
		method set_malla (a:Geo.malla)=
			malla <- Some a
	end;;

let contador_objeto_malla:c_objeto_malla General.agregador_observado=new General.agregador_observado;;
class objeto_malla=
	object(self)
		inherit c_objeto_malla
		initializer contador_objeto_malla#agregar (self:>c_objeto_malla)
	end;;
