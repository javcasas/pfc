let pi = 4.0 *. atan 1.0;;
let epsilon=0.00000000001;;

let rad_to_deg x=
	(180.0 /. pi) *. x;;

let deg_to_rad x=
	(pi /. 180.0) *. x;;

class vector (vec:float array)=
	object (self:'a)
		val vector=Array.copy vec
		method to_list=
			Array.to_list vector
		method to_array=
			Array.copy vector
		method num_elementos=
			Array.length vector
		method dimension=
			self#num_elementos
		method elemento i= (* primer elemento: 1 *)
			if 0 < i && i <= self#num_elementos then
				vector.(i - 1)
			else
				failwith ("Fuera de limites al ejecutar vector#elemento " ^ (string_of_int i) ^ " " ^(string_of_int self#num_elementos))
		method suma (v2:'a)= (* suma elemento a elemento *)
			match self#num_elementos == v2#num_elementos with
			false -> failwith ("No se pueden sumar dos vectores de distinta cantidad de elementos")
			|true -> 
				let l1= self#to_list in
				let l2= v2#to_list in
				let suma x y = x +. y in
				let res= List.map2 suma l1 l2 in
				let arr_res= Array.of_list res in
				{< vector=arr_res >}
		method producto_escalar i= (* producto por un escalar elemento a elemento *)
			let f x= x *. i in
			{< vector = Array.map f vector>}
		method producto_interior (v2:'a)= (* = sumatorio(ai * bi) *)
			match self#num_elementos == v2#num_elementos with
			false -> failwith ("No se pueden sumar dos vectores de distinta cantidad de elementos")
			|true -> 
				let l1= self#to_list in
				let l2= v2#to_list in
				let prod_parcial prev x y = (x *. y) +. prev in
				List.fold_left2 prod_parcial 0.0 l1 l2
		method longitud:float= (* sqrt(sum ai^2) *)
			let sum_parcial prev x= (x *. x) +. prev in
			let sum=Array.fold_left sum_parcial 0.0 vector in
			sqrt sum
			
		method a_unitario=
			let long=self#longitud in
			if long == 0.0 then
				failwith "No se puede crear un vector unitario a partir de uno de longitud 0"
			else
				self#producto_escalar (1.0 /. long)	

		method a_fila=
			new matriz [|self#to_array|]

		method a_columna=
			self#a_fila#traspuesta

		method fila_por_matriz (m2:matriz)=
			let m=self#a_fila in
			let res=m#matriz_por_matriz m2 in
			res#extrae_fila 1
			
		method producto_cruzado (v2:vector)=
			let x1,x2,x3,y1,y2,y3=
				match self#to_array, v2#to_array with
				| [|a;b;c|],[|d;e;f|] -> a,b,c,d,e,f
				| [|a;b|],[|d;e|] -> a,b,0.0,d,e,0.0
				|_->failwith "vector#producto cruzado: ambos vectores deben ser de 2 o 3 dimensiones" in

			let z1=(x2*.y3) -. (x3*.y2) in
			let z2=(x3*.y1) -. (x1*.y3) in
			let z3=(x1*.y2) -. (x2*.y1) in
			new vector [|z1;z2;z3|]
			
		method a_string=
			let res=ref (string_of_float (self#elemento 1)) in
			for i=2 to self#num_elementos do
				res := !res ^ " " ^ (string_of_float (self#elemento i))
			done;
			!res

	end


	and matriz (m:float array array)=
		(* Convenio: m.(i).(j) = elemento en fila i columna j *)
	let ()= if (Array.length m) == 0 then
		failwith "No se pueden crear matrices vacías. Deben tener algún elemento."
	else
		() in
	let tamx=Array.length m.(0) in
	let comprueba x=
		if (Array.length x) != tamx then
			failwith "No se pueden crear matrices si cada fila tiene una cantidad distinta de elementos!"
		else
			() in
	let ()=Array.iter comprueba m in
	let duplica_matriz t=
		let f x=Array.copy x in
		Array.map f m in
	object(self:'b)
		val mat=duplica_matriz m
		method num_filas=
			Array.length mat
		method num_columnas=
			Array.length mat.(0)
		method extrae_fila i=
			if 0 < i && i <= self#num_filas then
				new vector mat.(i - 1)
			else
				failwith "Fuera de limites al ejecutar self#num_fila"
		method extrae_columna j=
			if 0 < j && j <= self#num_columnas then
				(let col=Array.make self#num_filas 0.0 in
				let extractor indice e=
					mat.(indice).(j-1) in
				new vector (Array.mapi extractor col))
			else
				failwith "Fuera de limites al ejecutar self#extrae_columna"
		method extrae_elemento i j=
			if 0 < i && i <= self#num_filas && 0 < j && j <= self#num_columnas then
				mat.(i-1).(j-1)
			else
				failwith "Fuera de límites al ejecutar matriz#extrae_elemento"

		method extrae_submatriz (i1,i2) (j1,j2)=
			let res=Array.make_matrix (i2-i1+1) (j2-j1+1) 0.0 in
			for i=i1 to i2 do
				for j=j1 to j2 do
					res.(i-i1).(j-j1) <-  self#extrae_elemento i j
				done;
			done;
			{<mat=res>}


		method matriz_por_matriz (m:matriz)=
			(if self#num_columnas != m#num_filas then
				failwith "No se pueden multiplicar dos matrices con dimensiones incompatibles"
			else
				());
			let resultado=Array.make_matrix self#num_filas m#num_columnas 0.0 in
			for x = 1 to self#num_filas do
				for y = 1 to m#num_columnas do
					let fila=self#extrae_fila x in
					let columna=m#extrae_columna y in
					resultado.(x-1).(y-1) <- fila#producto_interior columna
				done;
			done;
			{<mat = resultado >}

		method matriz_por_columna (v:vector)=
			let m2=v#a_columna in
			let res=self#matriz_por_matriz m2 in
			res#extrae_columna 1
			
		method traspuesta=
			let res=Array.make_matrix self#num_columnas self#num_filas 0.0 in
			for x = 1 to self#num_filas do
				for y = 1 to self#num_columnas do
					res.(y-1).(x-1) <- self#extrae_elemento x y
				done;
			done;
			{< mat = res >}
		method cambia_elemento i j nv=
			mat.(i-1).(j-1) <- nv

		method duplica=
			new matriz mat
			(*{< mat = duplica_matriz mat >}*)

		method a_string=
			let res= ref ("|" ^(self#extrae_fila 1)#a_string^ "|") in
			for y=2 to self#num_filas do
				let v=self#extrae_fila y in
				res := !res ^ "\n|" ^ (self#extrae_fila y)#a_string^"|"
			done;
			!res
				
	end;;

class vector4 (v:float array)=
	let ()=(if Array.length v <> 4 then
			failwith "Creación vector4: el array entregado no tiene la longitud correcta"
		else ()) in
	vector v;;

class vector3 (v:float array)=
	let ()=(if Array.length v <> 3 then
			failwith "Creación vector3: el array entregado no tiene la longitud correcta"
		else ()) in
	vector v;;

class vector2 (v:float array)=
	let ()=(if Array.length v <> 2 then
			failwith "Creación vector2: el array entregado no tiene la longitud correcta"
		else ()) in
	vector v;;

class vector3_of_vector4 (v:vector4)=
	let [|x;y;z;w|]=v#to_array in
	vector [|x;y;z|];;

let matriz_cero i j=
	let arr=Array.make_matrix i j 0.0 in
	new matriz arr;;

let matriz_cuadrada_cero i=
	matriz_cero i i;;

let matriz_identidad i=
	let arr=Array.make_matrix i i 0.0 in
	for j=0 to i-1 do
		arr.(j).(j) <- 1.0
	done;
	new matriz arr;;

let matriz_translacion x y z=
	let m=matriz_identidad 4 in
	m#cambia_elemento 1 4 x;
	m#cambia_elemento 2 4 y;
	m#cambia_elemento 3 4 z;
	m
;;
let matriz_escala x y z=
	let m=matriz_identidad 4 in
	m#cambia_elemento 1 1 x;
	m#cambia_elemento 2 2 y;
	m#cambia_elemento 3 3 z;
	m
;;

let matriz_desde_filas (filas:vector array)=
	let ancho=(filas.(0))#num_elementos in
	let alto=Array.length filas in
	let m=Array.make_matrix ancho alto 0.0 in
	for i=0 to ancho - 1 do
		for j=0 to alto - 1 do
			try
				m.(i).(j) <- filas.(i)#elemento (j+1)
			with
				Failure(a) -> failwith ("matriz_desde_filas: las filas tienen longitud distinta:"^a)
		done
	done;
	new matriz m;;

let matriz_desde_columnas (columnas:vector array)=
	let matriz=matriz_desde_filas columnas in
	matriz#traspuesta;;
	

let vector4_i=
	new vector [|1.0; 0.0; 0.0; 0.0|];;
let vector4_j=
	new vector [|0.0; 1.0; 0.0; 0.0|];;
let vector4_k=
	new vector [|0.0; 0.0; 1.0; 0.0|];;
let vector4_l=
	new vector [|0.0; 0.0; 0.0; 1.0|];;
let vector4_cero=
	new vector [|0.0; 0.0; 0.0; 0.0|];;

let vector3_i=
	new vector [|1.0; 0.0; 0.0|];;
let vector3_j=
	new vector [|0.0; 1.0; 0.0|];;
let vector3_k=
	new vector [|0.0; 0.0; 1.0|];;
let vector3_cero=
	new vector [|0.0; 0.0; 0.0|];;


let vector2_i=
	new vector [|1.0;0.0|];;
let vector2_j=
	new vector [|0.0;1.0|];;


class cuaternion (v1:float) ((v2:float),(v3:float),(v4:float))=
	object(self)
		val a1=v1
		val a2=v2
		val a3=v3
		val a4=v4
		method valor=(a1,a2,a3,a4)
		method suma (c2:cuaternion)=
			let b1,b2,b3,b4=c2#valor in
			{< 
			a1 = a1 +. b1;
			a2 = a2 +. b2;
			a3 = a3 +. b3;
			a4 = a4 +. b4
			>}
		method conjugado = 
			{<
			a1 = a1;
			a2 = -. a2;
			a3 = -. a3;
			a4 = -. a4
			>}
		method producto (c2:cuaternion)=
			let a,b,c,d=a1,a2,a3,a4 in
			let t,x,y,z=c2#valor in
			{<
				a1=(a*.t) -. (b*.x) -. (c*.y) -. (d*.z);
				a2=(b*.t) +. (a*.x) +. (c*.z) -. (d*.y);
				a3=(c*.t) +. (a*.y) +. (d*.x) -. (b*.z);
				a4=(d*.t) +. (a*.z) +. (b*.y) -. (c*.x)
			>}
			
		method longitud=
			sqrt ((a1 *. a1) +. (a2 *. a2) +. (a3 *. a3) +. (a4 *. a4))
		method a_unitario=
			let l=self#longitud in
			if l <> 0.0 then
				{<
				a1 = a1 /. l;
				a2 = a2 /. l;
				a3 = a3 /. l;
				a4 = a4 /. l
				>}
			else
				failwith "El cuaternion es (0,0,0,0). No se puede hacer unitario."
		method extrae_rotacion=
			(*let c=self#a_unitario in*)
			(*let c=self in
			let a1,a2,a3,a4=c#valor in
			let vector=
				if (abs_float a1) > (1.0 -. epsilon) then
					new vector [|1.0;0.0;0.0|]
				else
				(*	(new vector [|a2;a3;a4|])#a_unitario in*)
					(new vector [|a2;a3;a4|]) in
			let ang= 2.0 *. acos a1 in*)
			let w,x,y,z = self#a_unitario#valor in
			let ang= (acos w) *. 2.0 in
			let cos_ang=w in
			let sin_ang=sqrt(1.0 -. (cos_ang *. cos_ang)) in
			let vector=
				if (abs_float sin_ang) < epsilon then
					new vector [|x;y;z|]
				else
					new vector [|x /. sin_ang;y /. sin_ang;z /. sin_ang|]
			in
			(vector, ang)
		method extrae_vector=
			if (abs_float a1) < epsilon then
				new vector [|a2;a3;a4|]
			else
				failwith "Cuaternion#extrae_vector: No es un vector (a1 != 0)"

		method a_matriz_rotacion=
			let w,x,y,z=a1,a2,a3,a4 in
			let xx = x *. x in
			let xy = x *. y in
			let xz = x *. z in
			let xw = x *. w in

			let yy = y *. y in
			let yz = y *. z in
			let yw = y *. w in

			let zz = z *. z in
			let zw = z *. w in
			
			let m=matriz_identidad 4 in

			m#cambia_elemento 1 1 (1.0 -. (2.0 *. (yy +. zz)));
			m#cambia_elemento 1 2 (        2.0 *. (xy -. zw));
			m#cambia_elemento 1 3 (        2.0 *. (xz +. yw));

			m#cambia_elemento 2 1 (        2.0 *. (xy +. zw));
			m#cambia_elemento 2 2 (1.0 -. (2.0 *. (xx +. zz)));
			m#cambia_elemento 2 3 (        2.0 *. (yz -. xw));

			m#cambia_elemento 3 1 (        2.0 *. (xz -. yw));
			m#cambia_elemento 3 2 (        2.0 *. (yz +. xw));
			m#cambia_elemento 3 3 (1.0 -. (2.0 *. (xx +. yy)));
			m
			
		method to_string=
			"(" ^ (string_of_float a1) ^ 
			",("^ (string_of_float a2) ^
			"," ^ (string_of_float a3) ^
			"," ^ (string_of_float a4) ^
			"))"
			
	end;;

let cuaternio_vector v=
	match v#to_array with
	| [|a;b;c|] -> new cuaternion 0.0 (a,b,c)
	| _ -> failwith "Mat.cuaternio_vector: el vector debe tener 3 componentes";;

let cuaternio_rotacion v ang=
	let ux, uy, uz = 
		match v#to_array with
		| [|a;b;c|] -> a,b,c
		| _ -> failwith "Mat.cuaternio_rotacion: el vector debe tener 3 componentes" in
	let c=cos (ang /. 2.0) in
	let s=sin (ang /. 2.0) in
	new cuaternion c ((s *. ux),(s *. uy),(s *. uz));;


let extrae_translacion m=
	let x=m#extrae_elemento 1 4 in
	let y=m#extrae_elemento 1 4 in
	let z=m#extrae_elemento 1 4 in
	new vector3 [|x;y;z|];;


let vector3_frontal=
	new vector [|0.0; 0.0; -1.0|];;
let vector3_vertical=
	new vector [|0.0; 1.0; 0.0|];;

let vector4_frontal=
	new vector [|0.0; 0.0; -1.0; 1.0|];;
let vector4_vertical=
	new vector [|0.0; 1.0; 0.0; 1.0|];;
