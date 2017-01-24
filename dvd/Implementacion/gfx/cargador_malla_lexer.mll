{
	open Cargador_malla_parser(* The type token is defined in parser.mli *)
	let conta=ref 0;;
}
rule token = parse
	| [' ' '\t']       	{ token lexbuf }     (* skip blanks *)
	| '\n' | '\r' | "\r\n"		{ incr conta; token lexbuf }     (* skip blanks *)
	| '#' [^'\n']* '\n'		{ token lexbuf } (* comentarios *)
	| '-'? ['0'-'9']+ '.' ['0'-'9']+ as lxm {REAL(float_of_string lxm)}
	| ['0'-'9']+ as lxm 		{ENTERO(int_of_string lxm)}
	| '"'[^'"']*'"' as cad		{CADENA(String.sub cad 1 ((String.length cad) - 2))}
	| "version"			{VERSION}
	| "o"				{OBJETO}
	| "nv"				{NUMVERTICES}
	| "v"				{VERTICE}
	| "nm"				{NUMMATERIALES}
	| "m"				{MATERIAL}
	| "tex"				{TEXTURA}
	| "notex"			{NOTEXTURA}
	| "amb"				{AMBIENTE}
	| "dif"				{DIFUSO}
	| "esp"				{ESPECULAR}
	| "emi"				{EMISION}
	| "nf"				{NUMCARAS}
	| "f"				{CARA}
	| "fm"				{MATERIAL_CARA}
	| "fv"				{VERTICE_CARA}
	| "fvuc"			{VERTICE_CARA_UV_COLOR}
	| eof    			{EOF}

