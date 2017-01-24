type estado=
	|Normal
	|Dormido
	|Congelado
	|Muerto;;

type señal=
	|Despertar
	|Dormir
	|Congelar
	|Matar;;



class virtual (*interfaz*) sistema=
	object(self)
		method virtual ejecutar_procesos:unit
		method virtual ejecutar_dormidos:unit
		method virtual añadir_proceso:proceso->unit
	end

	and virtual (*interfaz*)prioridad=
	object(self)
		method virtual valor:int
		method virtual ejecutar_procesos:unit
		method virtual ejecutar_dormidos:unit
		method virtual añadir_proceso:proceso->unit
		method virtual eliminar_proceso:proceso->unit
		method virtual pre_proceso:unit
		method virtual post_proceso:unit
	end
	and virtual (* interfaz *) proceso=
	object(self)
		method virtual estado:estado
		method virtual set_mi_prioridad:prioridad->unit
		method virtual prioridad:int
		method virtual set_prioridad:int->unit
		method virtual ejecutar:unit
		method virtual ejecutar_dormido:unit
		method virtual señal:señal->unit
		method virtual evento_despertar:unit
		method virtual evento_dormir:unit
		method virtual evento_congelar:unit
		method virtual evento_matar:unit
		method virtual lanzar:proceso->unit
	end;;

class prioridad_impl (i:int) (sis:sistema)=
	object(self)
		inherit prioridad
		val mutable normales=([]:proceso list)
		val mutable dormidos=([]:proceso list)
		val mutable congelados=([]:proceso list)
		(*val mutable muertos=([]:proceso list)*)
		val mutable normales_modificados=([]:proceso list)
		val mutable dormidos_modificados=([]:proceso list)
		val mutable congelados_modificados=([]:proceso list)
		(*val mutable muertos_modificados=([]:proceso list)*)

		val prioridad=i
		method valor=prioridad
		val mi_sistema=sis
		method ejecutar_procesos=
			let f x=x#ejecutar in
			List.iter f normales
		method ejecutar_dormidos=
			let f x=x#ejecutar_dormido in
			List.iter f dormidos
		method añadir_proceso (x:proceso)=
			if x#prioridad == prioridad then
				(x#set_mi_prioridad (self:>prioridad);
				match x#estado with
				|Normal -> normales_modificados <- x :: normales_modificados
				|Dormido -> dormidos_modificados <- x :: dormidos_modificados
				|Congelado -> congelados_modificados <- x :: congelados_modificados
				| _ -> () (* Descartamos los procesos muertos *))
			else
				mi_sistema#añadir_proceso x
		method eliminar_proceso x=
			let f y= y!=x in
			match x#estado with
			|Normal -> normales_modificados <- List.filter f normales_modificados
			|Dormido -> dormidos_modificados <- List.filter f dormidos_modificados
			|Congelado -> congelados_modificados <- List.filter f congelados_modificados
			|_-> ()
		method pre_proceso=()
		method post_proceso=
			(*let f x=x#estado != Muerto in
			procesos_modificados <- List.filter f procesos_modificados;*)
			normales<-normales_modificados;
			dormidos<-dormidos_modificados;
			congelados<-congelados_modificados
	end;;

class sistema_impl=
	object(self)
		inherit sistema
		val mutable prioridades=([]:prioridad list)
		method obten_prioridad (i:int)=
			try
				let f x=
					(x#valor)==i 
				in
				List.find f prioridades
			with
				Not_found -> 
					let nueva=((new prioridad_impl i (self:>sistema)):>prioridad) in
					prioridades <- nueva::prioridades;
					let criterio x y= y#valor - x#valor in
					prioridades <- List.sort criterio prioridades;					
					nueva
		method private pre_proceso=
			let f x= x#pre_proceso in
			List.iter f prioridades
		method private post_proceso=
			let f x= x#post_proceso in
			List.iter f prioridades
		method ejecutar_procesos=
			self#pre_proceso;
			let f x= x#ejecutar_procesos in
			List.iter f prioridades;
			self#post_proceso
		method ejecutar_dormidos=
			let f x= x#ejecutar_dormidos in
			List.iter f prioridades
		method añadir_proceso x=
			let prio=self#obten_prioridad (x#prioridad) in
			prio#añadir_proceso x
	end;;
			

class virtual proceso_abstract=
	object(self)
		inherit proceso
		val mutable estado=Normal		
		method estado=estado
		val mutable mi_prioridad=(None:prioridad option)
		method set_mi_prioridad p=
			mi_prioridad <- Some(p)
		val mutable prioridad=0
		method prioridad=prioridad
		method set_prioridad i=
			prioridad<-i;
			match mi_prioridad with
			|None -> ()
			|Some(a) -> 
				(if i==a#valor then
					()
				else
					(a#eliminar_proceso (self:>proceso); a#añadir_proceso (self:>proceso)))


		method virtual ejecutar:unit
		method ejecutar_dormido=()
		method evento_despertar=()
		method evento_dormir=()
		method evento_congelar=()
		method evento_matar=()
		method señal x=
			(match mi_prioridad with
			|None -> ()
			|Some(a) -> a#eliminar_proceso (self:>proceso));
			(match x with
			|Despertar ->
				estado<-Normal;
				self#evento_despertar
			|Dormir ->
				estado<-Dormido;
				self#evento_dormir
			|Congelar ->
				estado<-Congelado;
				self#evento_congelar
			|Matar ->
				estado<-Muerto;
				self#evento_matar);
			(match mi_prioridad with
			|None -> ()
			|Some(a) -> a#añadir_proceso (self:>proceso))
		method lanzar (x:proceso)=
			match mi_prioridad with
			|None -> ()
			|Some(a) -> a#añadir_proceso x
	end;;
