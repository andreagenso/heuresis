package com.heuresis.admin.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.common._

/*
 *  Objeto Opcion
 * */
object Opcion extends Opcion with LongKeyedMetaMapper[Opcion]
with CRUDify[Long, Opcion] {
	override def fieldOrder = List(id, nombre, nombrePagina)
}

/*
 *  Clase Opcion, representa todas las opciones de las aplicaciones
 * */

class Opcion extends LongKeyedMapper[Opcion] with IdPK {
	def getSingleton = Opcion

	// llave primaria
	def getId = this.id.toLong
	
	// LLave foranea de la aplicación
	object aplicacion_Id extends MappedLongForeignKey(this, Aplicacion){
		override def displayName = "Aplicacion_Id"
	}
	
	// Nombre de la aplicación
	object nombre extends MappedPoliteString(this, 50){
		override def displayName = "Nombre"
	}
	
	// llave de la opcion padre de la opción actual, sino tiene padre es 0
	object padre_id extends MappedLong(this) {
		override def displayName = "padre_id"
	}
	
	// True si la opción es Hoja, caso contrario False
	object esHoja extends MappedBoolean (this){
		override def displayName = "esHoja"
	}
	
	// Nombre de la opción, con la que se mostrará 
	object nombrePagina extends MappedPoliteString(this, 250){
		override def displayName = "pagina"
	}
	
	// nombre único, con el que que arma el mení
	object nombreUnico extends MappedPoliteString(this, 50){
		override def displayName = "nombreUnico"
	}
	
	// True Si tiene Reglas de reescritura en el Boot
	object esReescrito extends MappedBoolean (this){
		override def displayName = "esReescrito"
	}
	
	// determina el orden en el que aparece en el menú
	object orden extends MappedLong(this) {
		override def displayName = "orden"
	}
}