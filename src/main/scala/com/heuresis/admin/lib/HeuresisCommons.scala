package com.heuresis.admin.lib

import com.heuresis.admin.model.Aplicacion
/***
 *  Objeto que representa valores por defecto del Password de usuario
 * */
object DefaultPassword {
	val defaultPassword:String = "1234asdf"
		
	def getDefaultPassword:String = defaultPassword
}

/**
 *  Objeto que representa la unidad
 * */
object CommonsUnidad {
	val patronUnidadTraspaso = "U.*"
	val codAccesoOperDirecta = "D"
}

/**
 *  Objeto que representa la empresa
 * */
object CommonsEmpresa {
	val nombre = "CIDRE"
}

/**
 *  Objeto que representa el Sistema
 * */
object CommonsSistema {
	val headTitle = "Operación :0.9"
	val nombre = "Sistema Heuresis"
}


/**
 *  Objeto que representa la aplicacion Actual
 * */
object CommonsAplicacionActual {
	private val idAplicacion:Long = 2
	private val nombreAplicacion:String = "Operación"
		
	val idAplicacionActual = if (Aplicacion.findByKey(idAplicacion).get.nombre  == nombreAplicacion ) idAplicacion else 0
}

/* trait Paginador {

} */
