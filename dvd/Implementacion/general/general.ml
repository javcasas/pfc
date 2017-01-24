(* Patrón observador *)
class virtual observador=
	object(self)
		method virtual actualizar : unit
	end;;

class virtual sujeto=
	object(self)
		val mutable observadores=([] : observador list)
		method observadores=observadores
		method añadir_observador (o:observador)=
			if List.mem o observadores then
				()
			else
				observadores <- o :: observadores
		method eliminar_observador (o:observador)=
			let f x= (x!=o) in
			observadores <- List.filter f observadores
		method notificar = 
			let f x= x#actualizar in
			List.iter f observadores
	end;;
		

(* Agregador genérico *)
module Conjunto = Map.Make(
	struct 
		type t=int 
		let compare=Pervasives.compare 
	end);;

class ['a] agregador=
	object(self)
		(*inherit sujeto*)
		val mutable contenido=Conjunto.empty
		method agregar (x:'a)=
			contenido <- Conjunto.add (Oo.id x) x contenido
		method quitar (x:'a)=
			contenido <- Conjunto.remove (Oo.id x) contenido
		method pertenece (x:'a)=
			Conjunto.mem (Oo.id x) contenido
		method obtener_todos:'a list=
			let encadenar _ d prev=
				d :: prev in
			Conjunto.fold encadenar contenido []
	end;;

type 'a t_ultima_accion=
	|Agregar of 'a
	|Quitar of 'a;;

class ['a] agregador_observado=
	object(self)
		inherit ['a] agregador as agregador
		inherit sujeto as sujeto
		val mutable ultima_accion=None
		method ultima_accion=
			match ultima_accion with
			|Some(a) -> a
			|None -> failwith "agregador#ultima accion: No hubo última acción"
		method agregar x=
			agregador#agregar x;
			ultima_accion <- Some(Agregar(x));
			self#notificar

		method quitar x=
			agregador#quitar x;
			ultima_accion <- Some(Quitar(x));
			self#notificar
	end;;
			

(* Asociador genérico *)
module Asociador = Map.Make(
	struct
		type t=int
		let compare=Pervasives.compare
	end);;

	(* Asocia objetos de tipo 'a a objetos de tipo 'b *)
	(* Para cada objeto a hay un objeto b *)
class ['a, 'b] asociador=
	object(self)
		val mutable contenido=Asociador.empty
		method asociar (a:'a) (b:'b)=
			let clave=Oo.id a in
			contenido <- Asociador.add clave (a,b) contenido
		method quitar_asociacion (a:'a)=
			contenido <- Asociador.remove (Oo.id a) contenido
		method buscar_asociado (a:'a) : 'b=
			let clave=Oo.id a in
			try
				let a,b=Asociador.find clave contenido in
				b
			with
				Not_found -> failwith "asociador#buscar_asociado: no hay asociado a la clave indicada"
		method obtener_todos:('a*'b) list=
			let encadenar _ d prev=
				d :: prev in
			Asociador.fold encadenar contenido []
	end;;

(* Envoltorio para entero *)
class entero (n:int)=
	object(self)
		val valor=n
		method get=valor
	end;;
			

(* Objetos con destructor *)
class virtual destruible=
	object(self)
		method virtual destruir:unit
	end;;
