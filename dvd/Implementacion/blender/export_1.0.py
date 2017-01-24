from Blender import *

print "-----------------------------------"
import Blender
import string

class Exportador:
	materiales = {}
	def convierte_materiales(self, malla):
		contador = 0
		texto = ""
		if malla.hasFaceUV: # la malla puede tener imagenes
			caras = malla.faces
			for cara in caras:
				imagen = cara.image
				if imagen: #nueva imagen
					if imagen.filename not in self.materiales:
						#añadimos la nueva
						self.materiales[imagen.filename]="%d" % contador
						contador = contador + 1
						texto = texto + ("m \"%s\"\n" % imagen.filename)
						texto = texto + ("tex \"%s\"\n" % imagen.filename)
						texto = texto + "amb 0.2 0.2 0.2 1.0\n"
						texto = texto + "dif 0.8 0.8 0.8 1.0\n"
						texto = texto + "esp 0.0 0.0 0.0 1.0 0.0\n"
						texto = texto + "emi 0.0 0.0 0.0 1.0\n"
						texto = texto + "\n"
				else: #Sin textura
					if "notex" not in self.materiales:
						#añadimos la nueva
						self.materiales["notex"]="%d" % contador
						contador = contador + 1
						texto = texto + ("m \"notex\"\n")
						texto = texto + ("notex\n")
						texto = texto + "dif 0.8 0.8 0.8 1.0\n"
						texto = texto + "esp 0.0 0.0 0.0 1.0 0.0\n"
						texto = texto + "emi 0.0 0.0 0.0 1.0\n"
						texto = texto + "\n"
		else:
			self.materiales["notex"]="%d" % contador
			texto = texto + ("m \"notex\"\n")
			texto = texto + ("notex\n")
			texto = texto + "dif 0.8 0.8 0.8 1.0\n"
			texto = texto + "esp 0.0 0.0 0.0 1.0 0.0\n"
			texto = texto + "emi 0.0 0.0 0.0 1.0\n"

		texto = ("nm %d\n" % len(self.materiales)) + texto
		return texto
		
	def convierte_cara(self, malla, cara):
		texto = ""
		texto = texto + "f\n"
		vertices = cara.v
		if malla.hasFaceUV:
			imagen = cara.image
			if imagen:
				texto = texto + "fm " + self.materiales[imagen.filename] + "\n"
			else:
				texto = texto + "fm " + self.materiales["notex"] + "\n"
			indice=0
			for vertice in vertices:
				#print cara.uv
				uv=cara.uv[indice]
				(u,v)=uv
				#print cara.col
				col=cara.col[indice]
				r=col.r / 255.0
				g=col.g / 255.0
				b=col.b / 255.0
				a=col.a / 255.0
				texto = texto + ("fvuc %d %f %f %f %f %f %f\n" % (vertice.index, u, v, r, g, b, a))
				indice=indice + 1
		else:
			texto = texto + "fm 0\n"
			for indice, vertice in enumerate(vertices):
				texto = texto + ("fv %d\n" % (vertice.index))

		texto = texto + "\n"
		return texto
	
	def convierte_caras(self, malla, caras):
		texto = ""
		texto = texto + ("nf %d\n" % len(caras))
		for cara in caras:
			texto = texto + self.convierte_cara(malla, cara)
		return texto
	
	def convierte_vertice(self, malla, vertice):
		co=vertice.co
		no=vertice.no
		texto = "v %f %f %f %f %f %f\n" % (co.x, co.y, co.z, no.x, no.y, no.z)
		return texto
	
	def convierte_vertices(self, malla, vertices):
		texto = ""
		texto = texto + ("nv %d\n" % len(vertices))
		for vertice in vertices:
			texto = texto + self.convierte_vertice(malla, vertice)
		return texto
		
	def convierte_malla(self, nombre, malla):
		assert type(malla) == type(Blender.NMesh.New(""))
		
		vertices = malla.verts
		caras = malla.faces
		texto = ""
		texto = texto + self.convierte_vertices(malla, vertices) + "\n"
		texto = texto + self.convierte_materiales(malla) + "\n"
		texto = texto + self.convierte_caras(malla, caras) + "\n"
		return texto
	
	def exporta(self, objeto):

		texto = ""
		texto = texto + "#Exportado desde Blender 2.42\n"
		texto = texto + "version 1.0\n"

		nombre=objeto.getName()
		print nombre
		malla=NMesh.GetRawFromObject(nombre)

	
		texto = texto + ("o \"%s\"\n" % nombre)
		texto = texto + self.convierte_malla(nombre,malla)
		return (texto,1)
	


class Exportar:
	obj=None
	def exportar(self,filename):
		e=Exportador()
		if self.obj == None:
			print "Error"
			return
		(cadena,error) = e.exporta(self.obj)
		if error == 1:
			fh = open(filename, "w")
			fh.write(cadena)
			fh.close()
			#print(cadena)
			return
		else:
			print "Hubo algún error al exportar la malla"
			return


def main():
	editmode = Window.EditMode()    # are we in edit mode?  If so ...
	if editmode: Window.EditMode(0) # leave edit mode before getting the mesh

	objetos=Object.GetSelected()
	if objetos == []:
		print "No hay objetos seleccionados"
		return
	i = 0
	miexporta=Exportar()
	obj=None
	for objeto in objetos:
		if i == 0:
			obj=objeto
			i = 1
		else:
			print "Hay más de una malla seleccionada"
			return 
	miexporta.obj = obj

	Blender.Window.FileSelector(miexporta.exportar, 'Exportar fichero', 'd:\proyectos\pfc\pfc\implementacion\datos\sol.mesh')
	#miexporta.exportar("C:\pfc\implementacion\datos\malla.mesh")
	

	if editmode: Window.EditMode(1)  # optional, just being nice

main()
