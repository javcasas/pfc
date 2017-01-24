let fabrica_color r g b a=
	let fr=(float_of_int r) /. 255.0 in
	let fg=(float_of_int g) /. 255.0 in
	let fb=(float_of_int b) /. 255.0 in
	let fa=(float_of_int a) /. 255.0 in
	new Geo.color_simple (new Mat.vector4 [|fr;fg;fb;fa|]);;


(*class c_bitmap_RGB (dx:int) (dy:int) (d:string)=
	let ()=
		if String.length d != (dx*dy*3) then
			failwith "bitmap_RGB: la cadena no coincide con las dimensiones especificadas"
		else
			() in
	object(self)
		inherit Geo.bitmap
		val dim_x=dx
		val dim_y=dy
		val contenido=d
		method dim_x = dim_x
		method dim_y = dim_y
		method bits_por_pixel = 24
		method contenido = contenido
		method get_pixel (x,y) =
			if (x >= 0) && (dim_x > x) && (y >= 0) && (dim_y > y) then
				let pixel = String.sub contenido (((y*dim_y)+x)*3) 3 in
				let r=int_of_char pixel.[0] in
				let g=int_of_char pixel.[1] in
				let b=int_of_char pixel.[2] in
				fabrica_color r g b 255
			else
				failwith "bitmap_RGB#get_pixel: pixel fuera del bitmap"			
		method formato_color=
			(Int32.of_int (255 lsl 16),
			Int32.of_int (255 lsl 8),
			Int32.of_int 255,
			Int32.of_int 0)
	end;;

let contador_bitmap_RGB : c_bitmap_RGB General.agregador_observado=new General.agregador_observado;;

class bitmap_RGB (dx:int) (dy:int) (d:string)=
	object(self)
		inherit c_bitmap_RGB dx dy d
		initializer contador_bitmap_RGB#agregar (self :> c_bitmap_RGB)
	end;;
		*)

(* Almacena un bitmap por componentes RGB o RGBA *)
class c_bitmap_RGBA (dx:int) (dy:int) (bp:int) (d:string)=
	let ()=
		if String.length d != (dx*dy*bp/8) then
			failwith "bitmap_RGBA: la cadena no coincide con las dimensiones especificadas"
		else
			() in
	object(self)
		inherit Geo.bitmap
		val dim_x=dx
		val dim_y=dy
		val bpp=bp
		val contenido=d
		method dim_x = dim_x
		method dim_y = dim_y
		method bits_por_pixel = bpp
		method bytes_por_pixel = 
			(bpp / 8)
		method contenido = contenido
		method get_pixel (x,y) =
			let bpp=self#bytes_por_pixel in
			if (x >= 0) && (dim_x > x) && (y >= 0) && (dim_y > y) then
				let pixel=String.sub contenido (((y*dim_y)+x)*bpp) bpp in
				let r=int_of_char pixel.[0] in
				let g=int_of_char pixel.[1] in
				let b=int_of_char pixel.[2] in
				let a=
					match bpp with
					|3-> 255
					|4->int_of_char pixel.[3] in
				fabrica_color r g b a
			else
				failwith "bitmap_RGBA#get_pixel: pixel fuera del bitmap"			
		method formato_color=
			match self#bytes_por_pixel with
			|4->
				(Int32.of_int (255 lsl 24),
				Int32.of_int (255 lsl 16),
				Int32.of_int (255 lsl 8),
				Int32.of_int 255)
			|3->
				(Int32.of_int (255 lsl 16),
				Int32.of_int (255 lsl 8),
				Int32.of_int 255,
				Int32.of_int 0)
				
	end;;


let contador_bitmap_RGBA : c_bitmap_RGBA General.agregador_observado=new General.agregador_observado;;

class bitmap_RGBA (dx:int) (dy:int) (bp:int) (d:string)=
	object(self)
		inherit c_bitmap_RGBA dx dy bp d
		initializer contador_bitmap_RGBA#agregar (self :> c_bitmap_RGBA)
	end;;

let gestor_bitmaps:(Geo.bitmap Recursos.gestor_recursos)=new Recursos.gestor_recursos;;

type cad_bitmap=int * int * string * int;; (*ancho, alto, contenido, bits por pixel*)

let extrae_pixeles_24 s=
	let array_pixel=Sdlvideo.pixel_data_24 s in
	let pixeles=String.create (Bigarray.Array1.dim array_pixel) in
	let set_char (pos:int) (c:int)=
		let ch=char_of_int c in
		pixeles.[pos] <- ch in
	for i=0 to (Bigarray.Array1.dim array_pixel) - 1 do
		let ch=array_pixel.{i} in
		set_char i ch
	done;
	(*Invertimos rojo y azul?*)
	let swap pos=
		let r=pixeles.[pos] in
		pixeles.[pos] <- pixeles.[pos+2];
		pixeles.[pos+2] <- r in
	for i=0 to ((String.length pixeles)/3)-1 do
		swap (i*3)
	done;
	pixeles
	;;
	
let extrae_pixeles_32 s=
	let array_pixel=Sdlvideo.pixel_data_32 s in
	let pixeles=String.create ((Bigarray.Array1.dim array_pixel)*4) in
	let set_dword (pos:int) (px:int32)=
		(* Nota: invierte rojo y azul *)
		let ext_char n=
			let i=Int32.to_int (Int32.logand n (Int32.of_int 255)) in
			char_of_int i in
		(* Orden de bits: 32 .. 24 .. 16 .. 8 .. 0*)
		(*                  b4     b3    b2   b1 *)
		let b1=ext_char px in
		let px=Int32.shift_right_logical px 8 in
		let b2=ext_char px in
		let px=Int32.shift_right_logical px 8 in
		let b3=ext_char px in
		let px=Int32.shift_right_logical px 8 in
		let b4=ext_char px in

		pixeles.[pos] <- b3;
		pixeles.[pos + 1] <- b2;
		pixeles.[pos + 2] <- b1;
		pixeles.[pos + 3] <- b4
		in
	for i=0 to (Bigarray.Array1.dim array_pixel) - 1 do
		let px=array_pixel.{i} in
		set_dword (i*4) px
	done;
	(*Invertimos rojo y azul?*)
(*	let swap pos=
		let r=pixeles.[pos] in
		pixeles.[pos] <- pixeles.[pos+2];
		pixeles.[pos+2] <- r in
	for i=0 to ((String.length pixeles)/4)-1 do
		swap (i*4)
	done;*)
	pixeles
	;;


class cargador_cadena_bitmap (f:string)=
	object(self)
		inherit [cad_bitmap] Recursos.cargador
		val fichero=f
		val mutable contenido=(None:cad_bitmap option)
		method cargar=
			let s=Sdlloader.load_image f in
			let pixeles=
				match Sdlvideo.surface_bpp s with
				|24 -> extrae_pixeles_24 s
				|32 -> extrae_pixeles_32 s in

			let (w,h,_)=Sdlvideo.surface_dims s in
			contenido <- Some(w,h,pixeles,Sdlvideo.surface_bpp s);
			self#cambia_estado Recursos.Cargado
		method descargar=
			contenido <- None;
			self#cambia_estado Recursos.No_cargado
		method obtener=
			match contenido with
			|Some(a) -> a
			|None -> failwith "cargador_cadena_bitmap.obtener:No cargado"
		method clonar=
			let (w,h,original,bpp)=self#obtener in
			(w,h,String.copy original,bpp)
	end;;

class recurso_bitmap_RGBA (f:string)=
	let cargador=new cargador_cadena_bitmap f in
	object(self)
		inherit [cad_bitmap, bitmap_RGBA] Recursos.instanciador cargador
		method instanciar (w,h,str,bpp)=
			new bitmap_RGBA w h bpp str
	end;;

(*class recurso_bitmap_RGBA (f:string)=
	let cargador=new cargador_cadena_bitmap f in
	object(self)
		inherit [cad_bitmap, bitmap_RGBA] Recursos.instanciador cargador
		method instanciar (w,h,str)=
			new bitmap_RGBA w h str
	end;;*)
