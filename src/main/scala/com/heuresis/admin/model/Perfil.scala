package com.heuresis.admin.model

import _root_.net.liftweb.mapper._


import _root_.net.liftweb.http._
import SHtml._

import _root_.net.liftweb.util._
import Helpers._

import _root_.net.liftweb.common._
import _root_.scala.xml.{Text, Elem}

/*
 *  Representa la sesión actual del perfil
 * */
private object curPerfilId extends SessionVar[Box[String]](Empty)


/**
 * Clase Perfil
 * */
class Perfil extends LongKeyedMapper[Perfil] {
	def getSingleton = Perfil
	
	// llave primaria
	def primaryKeyField = id
	 
	object id extends MappedLongIndex(this) 
	
	// Llave foranea de aplicación
	object aplicacion_Id extends MappedLongForeignKey(this, Aplicacion) {
		override val fieldId = Some(Text("txtAplicacionId"))
		
		def _toForm(value:Boolean, perfilId:Long): Box[Elem] =
			if (value && 
				(UsuarioAplicacionPerfil.findAll(By(UsuarioAplicacionPerfil.perfil_Id, perfilId)).length > 0 || 
				OpcionPerfil.findAll(By(OpcionPerfil.perfil_Id, perfilId)).length > 0))
					{
					Full(SHtml.select(Aplicacion.findAll().  
							toList.sort(_.getDisplayName < _.getDisplayName).  
							map(lo => (lo.id.toString, lo.getDisplayName)),  
							Full(this.is.toString), setId) % ("id" -> fieldId) % ("disabled" -> "disabled") )
					
			} else {	Full(SHtml.select(Aplicacion.findAll().  
                    toList.sort(_.getDisplayName < _.getDisplayName).  
                    map(lo => (lo.id.toString, lo.getDisplayName)),  
                    Full(this.is.toString), setId) % ("id" -> fieldId) )
			}
                                    
        def setId(id:String) = {
        		this.set(id.toLong) }
        
        // Representa el Id actual del perfil
		def currentPerfilId: Box[String] = curPerfilId.is
		
		// Configurar la sesión actual del Perfil 
		def logPerfilIdIn(id: String) {
		    curPerfilId.remove()
		    curPerfilId(Full(id))
		}
		
		// Terminar la sesión del Perfil
		def logPerfilIdOut() {
			curPerfilId.remove()
		}
  } 
	
    /*def disabled(perfilId:Long):Boolean ={
		OpcionPerfil.findAll(By(OpcionPerfil.perfil_Id, perfilId)).length > 0 
	}*/

	// nombre del perfil
	object nombre extends MappedPoliteString(this, 50) {
		override def displayName = "Nombre"
	}
	
	// descripcion del perfil
	object descripcion extends MappedPoliteString(this, 250) {
		override def displayName = "Descripcion"
	}
	
	// devolver un perfil por defecto
	def getDefault():Perfil = {
		var p = new Perfil
		p.nombre("ninguno")
		p.descripcion("")
		p
	}
	
	// mostra nombre del perfil
	def getDisplayName():String ={
		this.nombre
	}
	
	// colocar id al perfil
	 def setIdPerfil(id:String) ={
	  this.id(id.toLong)
	  	
	}
}


/**
 * Objeto Perfil
 * */
object Perfil extends Perfil with KeyedMetaMapper[Long,Perfil]
                             with CRUDify[Long, Perfil]{
	
	/**
	 *  aplica indices definidos en la base de datos  
	 */
	override def dbIndexes = Index(id,aplicacion_Id) :: super.dbIndexes 

	   override def fieldOrder = List(id, nombre, descripcion)
	   override def viewMenuLoc = Empty
}