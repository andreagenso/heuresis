package bootstrap.liftweb

import com.heuresis.admin.lib.DBConnectionManagerMapper

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}
import _root_.com.heuresis.admin.model._
import _root_.com.heuresis.admin.lib._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
	  
     if (!DB.jndiJdbcConnAvailable_?) { 
     DB.defineConnectionManager(DefaultConnectionIdentifier, DBConnectionManagerMapper)
     S.addAround(DB.buildLoanWrapper)
    }

    // where to search snippet
    //LiftRules.addToPackages("com.heuresis.admin")
    //Schemifier.schemify(true, Schemifier.infoF _, User)

    // Build SiteMap
    //def sitemap() = SiteMap(
      //Menu("Home") / "index" :: // Simple menu form
      // Menu with special Link
      //Menu(Loc("Static", Link(List("static"), true, "/static/index"), 
	  //     "Static Content")) ::
      // Menu entries for the User management stuff
      //User.sitemap :_*)

    // LiftRules.setSiteMapFunc(sitemap)
    
   //var menu =  new MenuHeuresis()
    //var aplicacionId:Long = CommonsAplicacionActual.idAplicacionActual 
    //def sitemap() = { SiteMap(Usuario.sitemap ::: menu.getMenuPerfil( aplicacionId) :_*) }
    
    //LiftRules.setSiteMapFunc(sitemap)
    
   val entries =  OperMenu.listMenu
   LiftRules.setSiteMap(SiteMap(Usuario.sitemap ::: entries:_*))
   OperMenu.appendRewriteRules

   // menu.appendRewriteRules

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    //LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    S.addAround(DB.buildLoanWrapper)
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}
