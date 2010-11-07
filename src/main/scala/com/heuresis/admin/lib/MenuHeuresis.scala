package com.heuresis.admin.lib

import _root_.net.liftweb.util._

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import _root_.java.sql.{Connection, DriverManager}
import _root_.com.heuresis.admin.model._

import _root_.scala.xml.{NodeSeq, Node, Elem, PCData, Text}
import _root_.net.liftweb.http.js._
import JsCmds._
import JE._

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.sitemap._
import Loc._

import _root_.com.heuresis.admin.lib._

//class MenuHeuresis{
object OperMenu {      
	def validateSession = Usuario.currentUserId.isDefined &&
						Aplicacion.currentApplicationId.isDefined
										
	def validateLoggin = Usuario.loggedIn_?
	
	def validatePassword = if (Usuario.currentUserId.isDefined){ !(Usuario.findByKey(Usuario.currentUserId.get.toLong).get.password.match_?(DefaultPassword.getDefaultPassword)) }
							else true
							
	
	def validatePerfil(opcionId:Long) = if (validateSession) { val perfilId = UsuarioAplicacionPerfil.findAll(
    												By(UsuarioAplicacionPerfil.usuario_Id,Usuario.currentUserId.get.toLong), 
    												By(UsuarioAplicacionPerfil.aplicacion_Id, Aplicacion.currentApplicationId.get.toLong), 
    												MaxRows(1)).headOption match {
    													case None => 0
    														case Some(uap) => uap.perfil_Id.toLong
										      		}
	
											Log.info("PERFIL" + perfilId.toString)
											Log.info("OPCION" + opcionId.toString)
											Log.info("APLICACION" + Aplicacion.currentApplicationId.get.toLong)
											Log.info("USUARIO" + Usuario.currentUserId.get.toLong)
											
											OpcionPerfil.findAll(By(OpcionPerfil.perfil_Id, perfilId), By(OpcionPerfil.aplicacion_Id, Aplicacion.currentApplicationId.get.toLong), 
											By(OpcionPerfil.opcion_Id, opcionId)).length > 0													
		
						} else false
						
		
	def validateIngreso(opcionId:Long) = If (() => { validateLoggin && 
										validateSession &&
										validatePassword &&
										validatePerfil(opcionId)
    								}
    							,
    							() => RedirectResponse("/index"))	
	
	    							
   def getOpcionId(uniqueName:String):Long = {
	  val res = Opcion.findAll(By(Opcion.nombreUnico, uniqueName)).headOption match {
	 	  case None => 0
	 	  case Some(op) => op.id.toLong
	  }	
	   
	   Log.info("OPCION ID " + res)
	   
		res
	} 							
    							
    							
   val menuComprobanteList = Menu(Loc("ComprobanteList",
                             List("oper", "comprobante", "list"),
                             "Mantenimiento",validateIngreso(getOpcionId("ComprobanteList")) ))
                             
  val menuComprobantePendientes = Menu(Loc("ComprobanteListPendientes",
                             List("oper", "comprobante", "pendientes"),
                             "Pendientes",validateIngreso(getOpcionId("ComprobanteListPendientes")) ))
                             
  val menuComprobanteAutorizados = Menu(Loc("ComprobanteListAutorizados",
                             List("oper", "comprobante", "autorizados"),
                             "Autorizados", validateIngreso(getOpcionId("ComprobanteListAutorizados")) ))

  val menuComprobanteAdd = Menu(Loc("ComprobanteCreate",
                             List("oper", "comprobante", "create"),
                             "Agregar", validateIngreso(getOpcionId("ComprobanteCreate"))))
                             
  val submenusComprobante = List(menuComprobanteList, menuComprobantePendientes, menuComprobanteAutorizados)
  
  val menuComprobante = Menu(Loc("Comprobante",
                            List("oper", "comprobante") -> true,
                            "Comprobantes",
                            validateIngreso(getOpcionId("Comprobante"))),
                            submenusComprobante:_*)

  val listMenu = List(menuComprobante)

  def appendRewriteRules = LiftRules.rewrite.append {
    case RewriteRequest(
      ParsePath(List("oper", "comprobante", "view", id),_,_,_),_,_) =>
        RewriteResponse("oper" :: "comprobante" :: "view" :: Nil, Map("id" -> id))
    case RewriteRequest(
      ParsePath(List("oper", "comprobante", "viewapproved", id),_,_,_),_,_) =>
        RewriteResponse("oper" :: "comprobante" :: "viewapproved" :: Nil, Map("id" -> id))
    case RewriteRequest(
      ParsePath(List("oper", "comprobante", "autorizar", id),_,_,_),_,_) =>
        RewriteResponse("oper" :: "comprobante" :: "autorizar" :: Nil, Map("id" -> id))
    case RewriteRequest(
      ParsePath(List("oper", "comprobante", "create"),_,_,_),_,_) =>
        RewriteResponse("oper" :: "comprobante" :: "edit" :: Nil, Map("op" -> "create"))
    case RewriteRequest(
      ParsePath(List("oper", "comprobante", "edit", id),_,_,_),_,_) =>
        RewriteResponse("oper" :: "comprobante" :: "edit" :: Nil, Map("id" -> id))
    case RewriteRequest(
      ParsePath(List("oper", "comprobante", "delete", id),_,_,_),_,_) =>
        RewriteResponse("oper" :: "comprobante" :: "delete" :: Nil, Map("id" -> id))
  } 
	
}