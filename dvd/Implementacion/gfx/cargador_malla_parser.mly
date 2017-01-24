%{
	let contador=ref 0;;
	let coordenadas=ref [||];;
	let normales=ref [||];;
	let materiales_capas=ref [||];;
	let blanco= new Geo.color_simple (new Mat.vector4 [|1.0;1.0;1.0;1.0|]);;
	let vertices=ref [];;
	let materiales=ref [];;
	let bitmaps=ref [];;
	let inicializar ()=
		contador := 0;
		coordenadas := [||];
		normales := [||];
		vertices := [];
		materiales := [];
		materiales_capas := [||];
		bitmaps := [];;
%}

%token <float> REAL
%token <int> ENTERO
%token <string> CADENA
%token EOF
%token VERSION OBJETO NUMVERTICES VERTICE
%token NUMMATERIALES MATERIAL TEXTURA NOTEXTURA
%token AMBIENTE DIFUSO ESPECULAR EMISION NUMCARAS
%token CARA MATERIAL_CARA VERTICE_CARA VERTICE_CARA_UV_COLOR

%start main	/* the entry point */
%type < (Geo.malla_simple * string list) > main
%%

main:
	version nombre vertices materiales caras EOF 
	{ 
		(new Geo.malla_simple $5 (List.rev !vertices) (List.rev !materiales) , !bitmaps)
	}
;

version:
	VERSION REAL 		
	{ 
		inicializar();
		match $2 with
		|1.0 -> () (* Versión soportada *)
		|_ -> failwith "Versión del fichero no soportada"
	}
	;
nombre:
	OBJETO CADENA 		{ $2 }
	;
vertices:
	NUMVERTICES ENTERO desc_vertices 
	{ 
		let cos,nos=$3 in
	(*	let cos,nos=List.rev cos, List.rev nos in*)
		coordenadas := Array.of_list cos;
		normales := Array.of_list nos
	}
	;

desc_vertices:
	desc_vertice desc_vertices 
	{ 
		let co,no=$1 in
		let cos,nos=$2 in
		(co :: cos, no :: nos)
	}
	|desc_vertice 
	{ 
		let co,no=$1 in
		([co],[no])		
	}
	;
desc_vertice:
	VERTICE REAL REAL REAL REAL REAL REAL 
	{ 
		let co=new Geo.coordenada_simple (new Mat.vector3 [|$2;$3;$4|]) in
		let no=new Geo.normal_simple (new Mat.vector3 [|$5;$6;$7|]) in
		(co,no)
	}
	;

materiales:
	NUMMATERIALES ENTERO desc_materiales 
	{
	(*	let capas=List.rev $3 in*)
		let capas=$3 in
		let a_capas=Array.of_list capas in
		materiales_capas := a_capas
	}
	;

desc_materiales:
	desc_material desc_materiales 
	{
		$1 :: $2
	}
	|desc_material 
	{ 
		$1 :: []
	}
	;

desc_material:
	MATERIAL CADENA textura ambiente difuso especular emision 
	{ 
		let nombre=$2 in
		let textura=$3 in
		let amb=$4 in
		let dif=$5 in
		let (esp,f_esp)=$6 in
		let emi=$7 in
		let material=new Geo.material_simple amb dif (esp,f_esp) emi textura in
		materiales := material :: !materiales;
		let capa=new Geo.capa_simple material in
		capa
		
	}
	;
	
textura:
	TEXTURA CADENA 
	{
		(try 
			Bitmaps.gestor_bitmaps#get_item $2;
			()
		with
			Not_found -> 
				(let instanciador=new Bitmaps.recurso_bitmap_RGBA $2 in
				Bitmaps.gestor_bitmaps#nuevo_instanciador $2 (instanciador:>Geo.bitmap Recursos.a_instanciador);
				()));
		(*print_string $2;*)
		bitmaps := $2 :: !bitmaps;
		(new Geo.textura_bitmap $2 :> Geo.textura)
	}
	|NOTEXTURA 	
	{
		new Geo.no_textura
	}
	;

ambiente:
	AMBIENTE REAL REAL REAL REAL 
	{
		let c_ambiente=new Geo.color_simple (new Mat.vector4 [|$2;$3;$4;$5|]) in
		c_ambiente
	}
	;

difuso:
	DIFUSO REAL REAL REAL REAL 
	{
		let c_difuso=new Geo.color_simple (new Mat.vector4 [|$2;$3;$4;$5|]) in
		c_difuso
	}
	;

especular:
	ESPECULAR REAL REAL REAL REAL REAL 
	{
		let c_especular=new Geo.color_simple (new Mat.vector4 [|$2;$3;$4;$5|]) in
		(c_especular,$6)
	}
	;

emision:
	EMISION REAL REAL REAL REAL 
	{
		let c_emision=new Geo.color_simple (new Mat.vector4 [|$2;$3;$4;$5|]) in
		c_emision
	}
	;
	
caras:
	NUMCARAS ENTERO desc_caras 
	{
		List.rev $3
	}
	;
	
desc_caras:
	desc_cara desc_caras { $1 :: $2 }
	|desc_cara { $1 :: [] }
	;

desc_cara:
	CARA material_cara vertices_cara 
	{ 
		let capa=$2 in
		let vertices=$3 in
		new Geo.poligono_simple vertices [capa]
	}
	;

material_cara:
	MATERIAL_CARA ENTERO 
	{ 
	       ( !materiales_capas ).($2)	
	}
	;

vertices_cara:
	vertice_cara vertice_cara vertice_cara { [$1;$2;$3] }
	| vertice_cara vertice_cara vertice_cara vertice_cara { [$1;$2;$3;$4] }
	;

vertice_cara:
	VERTICE_CARA ENTERO 
	{ 
		let co=( !coordenadas ).($2) in
		let no=( !normales ).($2) in
		let color=blanco in
		let v=new Geo.vertice_simple co no color [] in
		vertices := v :: !vertices;
		v
	}
	|VERTICE_CARA_UV_COLOR ENTERO REAL REAL REAL REAL REAL REAL 
	{ 
		let co=( !coordenadas ).($2) in
		let no=( !normales ).($2) in
		(*Invertimos la coordenada vertical Uy*)
		let uv=new Geo.uv_simple (new Mat.vector2 [|$3;-.$4|]) in 
		let col=new Geo.color_simple (new Mat.vector4 [|$5;$6;$7;$8|] ) in
		let v=new Geo.vertice_simple co no col [uv] in
		vertices := v :: !vertices;
		v
		
	}
	;

