class virtual motor=
	object(self)
		method virtual inicializar : unit
		method virtual dibujarFotograma : unit
		method virtual terminar : unit
	end;;

class proxy_motor t=
	object(self)
		inherit motor
		val mutable motor_final=(None:motor option)
		val nombre_mensaje="proxy_motor_"^t
		method set_motor_final x=
			motor_final <- x
		method get_motor_final=
			match motor_final with
			|Some(a) -> a
			|None -> failwith (nombre_mensaje^": no existe el motor final")
		method inicializar=
			self#get_motor_final#inicializar
		method dibujarFotograma=
			self#get_motor_final#dibujarFotograma
		method terminar=
			self#get_motor_final#terminar
	end;;

let motor_grafico=new proxy_motor "grafico";;
let motor_sonido=new proxy_motor "sonido";;

class gestor_motores=
	object(self)
		val mutable motores_disponibles=([]:(string*motor) list)
		method obten_motor n=
			try
				List.assoc n motores_disponibles
			with
				Not_found -> failwith ("gestor_motores#obten_motor: No hay un motor disponible llamado "^n)
		method registra_motor nombre motor=
			if not (List.mem_assoc nombre motores_disponibles) then
				motores_disponibles <- (nombre,motor) :: motores_disponibles
			else
				failwith ("gestor_motores#registra_motor: Nombre duplicado: "^nombre)
	end;;

let motores_graficos=new gestor_motores;;
let motores_sonido=new gestor_motores;;
