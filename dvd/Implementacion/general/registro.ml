(* registro.ml : Guarda los registros de incidencias de ejecución *)

type t_registro=
	|Mensaje of string
	|Advertencia of string
	|Error of string;;

let mensajes=ref ([] : t_registro list);;

let append_mesg m=
	mensajes := (m :: !mensajes);;

let mensaje s=
	append_mesg (Mensaje(s));;

let advertencia s=
	append_mesg (Advertencia(s));;

let error s=
	append_mesg (Error(s));;

let str_mesgs ()=
	let str_mesg m=
		match m with
		|Mensaje(s) -> 		s
		|Advertencia(s) -> 	("Advertencia: "^s)
		|Error(s) -> 		("**ERROR**: "^s)
	in
	let cad_res=ref "" in
	let f m=
		let cad = str_mesg m in
		cad_res := !cad_res ^ cad ^ "\n" in
	List.iter f (List.rev !mensajes);
	!cad_res;;

let print_mesgs ()=
	print_string (str_mesgs());;

(*Op a la salida*)
at_exit print_mesgs;;
