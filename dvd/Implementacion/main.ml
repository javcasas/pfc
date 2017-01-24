class main_loop=
	object(self)
		val mutable seguir_bucle=true
		val sistema_procesos=(new Procesos.sistema_impl:>Procesos.sistema)		
		method sistema_procesos=sistema_procesos

		method fin_bucle=
			seguir_bucle <- false
			
		method accion_tecla tecla=
			match tecla with
			| Sdlkey.KEY_ESCAPE | Sdlkey.KEY_q -> self#fin_bucle
			| _ -> ()

		method leer_entrada=
			while Sdlevent.has_event () do
				match Sdlevent.wait_event () with
				| Sdlevent.KEYDOWN { Sdlevent.keysym = tecla } -> 
					self#accion_tecla tecla
				| Sdlevent.QUIT -> self#fin_bucle
				| _ -> ()
			done

		method ejecutar_procesos=
			sistema_procesos#ejecutar_procesos

		method ejecutar_motor_grafico=
			Vista.motor_grafico#dibujarFotograma

		method ejecutar_motor_sonido=
			()
			
		method mostrar_resultados=
			self#ejecutar_motor_grafico;
			self#ejecutar_motor_sonido
			
		method ejecutar=
			while(seguir_bucle) do
				self#leer_entrada;
				self#ejecutar_procesos;
				self#mostrar_resultados;
				Sdltimer.delay 1
			done
	end;;

(*let main_loop=new main_loop;;*)

class main=
	object(self)
		method inicializar_SDL=
			Sdl.init [`VIDEO];
			Sdlwm.set_caption ~title: "PFC" ~icon: "PFC"
			
		method detener_SDL=
  			Sdl.quit ()
			
			
		method inicializar_motor_grafico=
			let motor=Vista.motores_graficos#obten_motor "OpenGL" in
			Vista.motor_grafico#set_motor_final (Some(motor));
			Vista.motor_grafico#inicializar
			
		method detener_motor_grafico=
			Vista.motor_grafico#terminar
			
		method inicializar_motor_sonido=
			()
		method detener_motor_sonido=
			()
			
		method lanzar_proceso_base b=
			let sistema=b#sistema_procesos in
			sistema#añadir_proceso Logica.proceso_base


		method ejecutar=
			self#inicializar_SDL;
			self#inicializar_motor_grafico;
			self#inicializar_motor_sonido;

                        let bucle_principal = new main_loop in			
			self#lanzar_proceso_base bucle_principal;

			bucle_principal#ejecutar;

			self#detener_motor_sonido;
			self#detener_motor_grafico;
			self#detener_SDL
	end;;
			
let main = new main;;

let _=main#ejecutar;;
