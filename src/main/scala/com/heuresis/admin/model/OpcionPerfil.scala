package com.heuresis.admin.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.common._

/*
 * Clase OpcionPerfil
 * */
class OpcionPerfil extends  LongKeyedMapper[OpcionPerfil] with IdPK {
	def getSingleton = OpcionPerfil

	// llave primaria
	def getId = this.id.toLong	
	
	// Llaves foraneas de perfil, opcion, aplicacion
	object perfil_Id extends MappedLongForeignKey(this, Perfil)
	object opcion_Id extends MappedLongForeignKey(this, Opcion)
	object aplicacion_Id extends MappedLongForeignKey(this, Aplicacion)
	
	// funcion de borrado para OpcionPerfil con llave compuesta
	 /*def deleteOpcionPerfil(idPerfil : Long) = {
		    DB.use(DefaultConnectionIdentifier ) { 
		    	conn => DB.prepareStatement("DELETE FROM opcion_perfil WHERE perfil_id = " + idPerfil + ";", conn) { sta => sta.executeUpdate }
		    }
	}	*/
}

/*
 * Objeto OpcionPerfil
 * */
object OpcionPerfil extends OpcionPerfil with LongKeyedMetaMapper[OpcionPerfil]{
	override def dbTableName = "opcion_perfil" // define the DB table name
	override def fieldOrder = List(perfil_Id, opcion_Id, aplicacion_Id)	
}