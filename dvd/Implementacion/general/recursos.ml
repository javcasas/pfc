type estado=
	|No_cargado
	|Carga_incompleta of float
	|Cargado
	|Descarga_incompleta of float
	|Error_carga of string
	|Error_descarga of string;;


let mutex=Mutex.create ();;
let mutex_op f=
	Mutex.lock mutex;
	let res=f () in
	Mutex.unlock mutex;
	res;;


class virtual ['a] cargador=
	object(self)
		val mutable estado=(No_cargado:estado)
		method estado:(estado)=
			mutex_op (fun ()->estado)
		method cambia_estado (n : estado)=
			mutex_op (fun ()->estado <- n)
		method virtual cargar : unit
		method virtual descargar : unit
		method virtual obtener : 'a
		method virtual clonar : 'a
	end;;

class virtual ['obj] a_instanciador =
	object(self)
		method virtual cargar:unit
		method virtual descargar:unit
		method virtual obtener:'obj
		method virtual obtener_duplicado:'obj
		method virtual estado:estado
	end;;

class virtual ['datos,'obj] instanciador (c:'datos cargador)=
	object(self)
		val mi_cargador=c
		method mi_cargador=mi_cargador
		inherit ['obj] a_instanciador
		method estado=
			self#mi_cargador#estado
		method cargar=
			self#mi_cargador#cargar
		method descargar=
			self#mi_cargador#descargar
		method virtual instanciar: 'datos -> 'obj
		method obtener=
			match self#estado with
			|Cargado -> 
				let datos=self#mi_cargador#obtener in
				self#instanciar datos
			|_-> failwith "recurso#obtener: el recurso no está cargado"
		method obtener_duplicado= (* Devuelve una copia independiente del recurso *)
			match self#estado with
			|Cargado -> 
				let datos=self#mi_cargador#clonar in
				self#instanciar datos
			|_-> failwith "recurso_hilos#obtener_duplicado: el recurso no está cargado"
	end;;

class virtual ['datos,'obj] instanciador_hilos (c:'datos cargador)=
	object(self)
		inherit ['obj] a_instanciador
		
		val mi_cargador=c
		method mi_cargador=mi_cargador

		val mutable hilo=None
		method estado=
			self#mi_cargador#estado
			
		method cargar=	(* Comienza la carga del recurso *)
			hilo <- Some (Thread.create (fun () -> self#mi_cargador#cargar) ())
				
		method descargar= 	(* Descarga el recurso *)
			hilo <- Some (Thread.create (fun () -> self#mi_cargador#descargar) ())
	
		method virtual instanciar: 'datos -> 'obj

		method obtener=	(* Devuelve el recurso, o falla si no está cargado *)
			match self#estado with
			|Cargado -> 
					let datos=self#mi_cargador#obtener in
					self#instanciar datos
			|_-> failwith "recurso_hilos#obtener: el recurso no está cargado"

		method obtener_duplicado= (* Devuelve una copia independiente del recurso *)
			match self#estado with
			|Cargado -> 
					let datos=self#mi_cargador#clonar in
					self#instanciar datos
			|_-> failwith "recurso_hilos#obtener_duplicado: el recurso no está cargado"
	end;;

(**)
module CadenaOrdenada =
	struct
		type t = string
		let compare=Pervasives.compare
	end;;

module GestorRecursos = Map.Make(CadenaOrdenada);;
(**)
class ['a] gestor_recursos=
	object(self)
		val mutable contenido = GestorRecursos.empty
		method nuevo_instanciador (id:string) (ob:'a a_instanciador)=
			if not (GestorRecursos.mem id contenido) then
				contenido <- GestorRecursos.add id ob contenido
			else
				failwith "Identificador repetido"
		method get_item id=
			GestorRecursos.find id contenido
			
		method cargar id=
			let recurso = self#get_item id in
			match recurso#estado with
			|No_cargado |Error_carga(_) -> recurso#cargar
			|_->failwith "gestor_recursos#cargar: no se puede lanzar la carga"
			
		method obtener id=
			let recurso=self#get_item id in 
			recurso#obtener
			
		method descargar id=
			let recurso = self#get_item id in
			match recurso#estado with
			|Cargado | Error_descarga(_) -> recurso#descargar
			|_->failwith "gestor_recursos#descargar: no se puede lanzar la descarga"
			
		method estado id=
			let recurso = self#get_item id in
			recurso#estado

		method cargado id=
			let est=self#estado id in
			match est with
			|Cargado -> true
			|_->false

		method obtener_duplicado id=
			let recurso = self#get_item id in
			recurso#obtener_duplicado

		method forzar_obtener id=
				(match self#estado id with
				|No_cargado -> self#cargar id
				| _-> ());
				let cargando ()=
					match self#estado id with
					|Carga_incompleta(_) -> true
					|_->false in

				while cargando () do
					()
				done;
				match self#estado id with
				|Cargado -> self#obtener id
				|_ -> failwith "Gestor_recursos#forzar_obtener: No se puede obtener el recurso" 

	end;;
