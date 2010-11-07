package com.heuresis.admin.model

import _root_.net.liftweb.mapper._

import _root_.net.liftweb.common._

/**
 * Clase UsuarioAplicacionPerfil
 * */
class UsuarioAplicacionPerfil extends  LongKeyedMapper[UsuarioAplicacionPerfil] {
	def primaryKeyField = id 

	def getSingleton = UsuarioAplicacionPerfil
	def getId = this.id.toLong
	object id extends MappedLongIndex(this) 

	object usuario_Id extends MappedLongForeignKey(this, Usuario)
	object perfil_Id extends MappedLongForeignKey(this, Perfil)
	object aplicacion_Id extends MappedLongForeignKey(this, Aplicacion)
}

/**
 * Objeto UsuarioAplicacionPerfil
 * */
object UsuarioAplicacionPerfil extends UsuarioAplicacionPerfil with LongKeyedMetaMapper[UsuarioAplicacionPerfil]{
	override def dbIndexes = Index(usuario_Id, perfil_Id, aplicacion_Id) :: super.dbIndexes 

	override def dbTableName = "usuario_aplicacion_perfil" // define the DB table name
	override def fieldOrder = List( usuario_Id, aplicacion_Id)	
}