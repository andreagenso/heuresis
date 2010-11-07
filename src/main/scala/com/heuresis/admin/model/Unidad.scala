package com.heuresis.admin.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.common._

/*
 *  Objeto Unidad
 * */
object Unidad extends Unidad with LongKeyedMetaMapper[Unidad]
with CRUDify[Long, Unidad] {
	override def fieldOrder = List(id, codigo, nombre, nivelSuperior_Id, codAccesoOper,nivel, codHabilitacion, lugar_Id)
}

/*
 *  Clase Unidad
 * */
class Unidad extends LongKeyedMapper[Unidad] with IdPK {
	def getSingleton = Unidad

	def getId = this.id.toLong
	
	object codigo extends MappedPoliteString(this, 32){
		override def displayName = "Codigo"
	}
	
	object nombre extends MappedPoliteString(this, 64){
		override def displayName = "Nombre"
	}
	
	object nivelSuperior_Id extends MappedLong(this) {
		override def displayName = "nivelSuperior_id"
	}
	
	object codAccesoOper extends MappedPoliteString(this, 8){
		override def displayName = "Nombre"
	}
	
	object nivel extends MappedInt(this) {
		override def displayName = "Nivel"
	}
	
	object codHabilitacion extends MappedPoliteString(this, 8){
		override def displayName = "Cod Habilitacion"
	}
	
	object lugar_Id extends MappedLong(this) {
		override def displayName = "nivelSuperior_id"
	}

	def getDisplayName():String ={
		this.nombre
	}
}