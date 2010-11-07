package com.heuresis.admin.model

import net.liftweb.http.SessionVar


import _root_.net.liftweb.mapper._
import _root_.net.liftweb.common._


/*
 *  Objeto Aplicacion

 * */
object Aplicacion extends Aplicacion with LongKeyedMetaMapper[Aplicacion]
with CRUDify[Long, Aplicacion] {
	override def fieldOrder = List(id, nombre, descripcion, ruta)
}

/*
 *  Representa la sesión actual de la aplicación
 * */
private object curApplicationId extends SessionVar[Box[String]](Empty)

/*
 *  Clase Aplicación
 * */
class Aplicacion extends LongKeyedMapper[Aplicacion] with IdPK {
	def getSingleton = Aplicacion
	
	// llave primaria
	def getId = this.id.toLong
	
	// nombre de la aplicación
	object nombre extends MappedPoliteString(this, 50){
		override def displayName = "Nombre"
	}
	
	// descripción de la aplicación
	object descripcion extends MappedPoliteString(this, 250){
		override def displayName = "Descripción"
	}
	
	// ruta de la aplicación
	object ruta extends MappedPoliteString(this, 250){
		override def displayName = "Ruta"
	}
	
	object orden extends MappedLong(this)
	
	// retornar aplicación por defecto
	def getDefault():Aplicacion = {
		var ap = new Aplicacion
		ap.nombre("ninguno")
		ap.descripcion("")
		ap
	}
	
	// Mostrar el nombre de la aplicación
	def getDisplayName():String ={
		this.nombre
	}	
	
	// Representa el Id actual de la aplicación
	def currentApplicationId: Box[String] = curApplicationId.is
	
	// Configurar la sesión actual de la aplicación 
	def logApplicationIdIn(id: String) {
	    curApplicationId.remove()
	    curApplicationId(Full(id))
	}
	
	// Terminar la sesión de la aplicación
	def logApplicationIdOut() {
		curApplicationId.remove()
	}
}