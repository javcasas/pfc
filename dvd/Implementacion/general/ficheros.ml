class virtual dispositivo_entrada_caracteres=
	object(self)
		method virtual leer_caracter:char
		method virtual leer_linea  : string
		method virtual leer_cadena : int -> (string* int)
		(* Lee [0-n] caracteres *)
		(* Devuelve la cadena y los caracteres leídos *)
		method virtual leer_cadena_completa : int -> string
		(* Lee n caracteres, o lanza una excepción Eof si no puede *)
		method virtual longitud    : int
		method virtual posicion_lectura : int
		method virtual set_posicion_lectura : int -> unit
		method destroy=()
	end;;

class fichero_entrada (fn:string)=
	object(self)
		inherit dispositivo_entrada_caracteres
		val archivo=open_in_bin fn
		method leer_caracter=
			input_char archivo
		method leer_linea=
			input_line archivo
		method longitud=
			in_channel_length archivo
		method leer_cadena i =
			let cadena=String.create i in
			let n=input archivo cadena 0 i in
			if n == i then
				(cadena, n)
			else
				(String.sub cadena 0 n,n)
		method leer_cadena_completa i=
			let cadena=String.create i in
			really_input archivo cadena 0 i;
			cadena
		method posicion_lectura=
			pos_in archivo
		method set_posicion_lectura i=
			seek_in archivo i
		method destroy=
			close_in archivo
	end;;

