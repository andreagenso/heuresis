package com.heuresis.admin.model

import _root_.net.liftweb.mapper._

import _root_.net.liftweb.http._
import js._
import JsCmds._
import _root_.scala.xml.{NodeSeq, Node, Text, Elem}
import _root_.scala.xml.transform._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util.Mailer._
import S._
import _root_.com.heuresis.admin.lib._

// 1. trait ProtoUserPerfil
trait ProtoUserPerfil[T <: ProtoUserPerfil[T]] extends KeyedMapper[Long, T] with UserIdAsString {
  self: T =>

  override def primaryKeyField = id

  // the primary key for the database
  object id extends MappedLongIndex(this)

  def userIdAsString: String = id.is.toString

  // First Name
  object firstName extends MappedString(this, 32) {
    override def displayName = fieldOwner.firstNameDisplayName
    override val fieldId = Some(Text("txtFirstName"))
  }
  def firstNameDisplayName = ??("Nombre")

  // Last Name
  object lastName extends MappedString(this, 32) {
    override def displayName = fieldOwner.lastNameDisplayName
    override val fieldId = Some(Text("txtLastName"))
  }

  def lastNameDisplayName = ??("Apellidos")

  // Email
  object email extends MappedEmail(this, 48) {
    override def dbIndexed_? = true
    override def validations = valUnique(S.??("unique.email.address")) _ :: super.validations
    override def displayName = fieldOwner.emailDisplayName
    override val fieldId = Some(Text("txtEmail"))
  }

  def emailDisplayName = ??("Correo electrónico")
  // Password
  object password extends MappedPassword[T](this) {
    override def displayName = fieldOwner.passwordDisplayName
  }

  def passwordDisplayName = ??("Contraseña")

  object superUser extends MappedBoolean(this) {
    override def defaultValue = false
  }

  def niceName: String = (firstName.is, lastName.is, email.is) match {
    case (f, l, e) if f.length > 1 && l.length > 1 => f+" "+l+" ("+e+")"
    case (f, _, e) if f.length > 1 => f+" ("+e+")"
    case (_, l, e) if l.length > 1 => l+" ("+e+")"
    case (_, _, e) => e
  }

  def shortName: String = (firstName.is, lastName.is) match {
    case (f, l) if f.length > 1 && l.length > 1 => f+" "+l
    case (f, _) if f.length > 1 => f
    case (_, l) if l.length > 1 => l
    case _ => email.is
  }

  def niceNameWEmailLink = <a href={"mailto:"+email.is}>{niceName}</a>
}

// 2. trait MegaProtoUserPerfil
trait MetaMegaProtoUserPerfil[ModelType <: MegaProtoUserPerfil[ModelType]] extends KeyedMetaMapper[Long, ModelType] {
  self: ModelType =>

  // def signupFields: List[BaseOwnedMappedField[ModelType]] = firstName :: lastName :: email :: locale :: timezone :: password :: perfil_Id :: Nil
  def signupFields: List[BaseOwnedMappedField[ModelType]] = firstName :: lastName :: email :: locale :: timezone :: password ::  unidad_Id :: Nil

  // override def fieldOrder: List[BaseOwnedMappedField[ModelType]] = firstName :: lastName :: email :: locale :: timezone :: password :: perfil_Id :: Nil
   override def fieldOrder: List[BaseOwnedMappedField[ModelType]] = firstName :: lastName :: email :: locale :: timezone :: password :: unidad_Id :: Nil

  /**
   * If the
   */
  def screenWrap: Box[Node] = Empty

  val basePath: List[String] = "user_mgt" :: Nil
  def signUpSuffix = "sign_up"
  lazy val signUpPath = thePath(signUpSuffix)
  def loginSuffix = "login"
  lazy val loginPath = thePath(loginSuffix)
  def lostPasswordSuffix = "lost_password"
  lazy val lostPasswordPath = thePath(lostPasswordSuffix)
  def passwordResetSuffix = "reset_password"
  lazy val passwordResetPath = thePath(passwordResetSuffix)
  def changePasswordSuffix = "change_password"
  lazy val changePasswordPath = thePath(changePasswordSuffix)
  def logoutSuffix = "logout"
  lazy val logoutPath = thePath(logoutSuffix)
  def editSuffix = "edit"
  lazy val editPath = thePath(editSuffix)
  def validateUserSuffix = "validate_user"
  lazy val validateUserPath = thePath(validateUserSuffix)

  def homePage = "/"

  object loginRedirect extends SessionVar[Box[String]](Empty)

  case class MenuItem(name: String, path: List[String],
                      loggedIn: Boolean) {
    lazy val endOfPath = path.last
    lazy val pathStr: String = path.mkString("/", "/", "")
    lazy val display = name match {
      case null | "" => false
      case _ => true
    }
  }

  def thePath(end: String): List[String] = basePath ::: List(end)

  /**
   * Return the URL of the "login" page
   */
  def loginPageURL = loginPath.mkString("/","/", "")

  def notLoggedIn_? = !loggedIn_?

  lazy val testLogginIn = If(loggedIn_? _, S.??("Usuario no ha ingresado")) ;  // Debe ingresar

  lazy val testSuperUser = If(superUser_? _, S.??("Debe ser super usuario"))  // Debe ser super usuario
  
  def loginFirst = If(
    loggedIn_? _,
    () => {
      import _root_.net.liftweb.http.{RedirectWithState, RedirectState, LiftResponse}
      val uri = S.uriAndQueryString
      RedirectWithState(
        loginPageURL,
        RedirectState( ()=>{loginRedirect.set(uri)})
      )

    }
  )
  
   
  def superUser_? : Boolean = currentUser.map(_.superUser.is) openOr false

  /**
   * The menu item for login (make this "Empty" to disable)
   */
  //login
  def loginMenuLoc: Box[Menu] =
    Full(Menu(Loc("Login", loginPath, S.??("Ingresar"), loginMenuLocParams)))

  def homeMenuLoc: Box[Menu] =
    Full( Menu(Loc("0", "index" :: Nil,
                            "Inicio" )))
                            

  /**
   * The LocParams for the menu item for login.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
    // already.logged.in
  protected def loginMenuLocParams: List[LocParam[Unit]] =
    If(notLoggedIn_? _, S.??("Conectado")) ::  // Ya esta conectado
    Template(() => wrapIt(login)) ::
    Nil

  /**
   * The menu item for logout (make this "Empty" to disable)
   */
    // logout
  def logoutMenuLoc: Box[Menu] =
    Full(Menu(Loc("Logout", logoutPath, S.??("Salir"), logoutMenuLocParams)))

  /**
   * The LocParams for the menu item for logout.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def logoutMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(logout)) ::
    testLogginIn ::
    Nil

  /**
   * The menu item for creating the user/sign up (make this "Empty" to disable)
   */
    // sign.up
  def createUserMenuLoc: Box[Menu] =
    Full(Menu(Loc("CreateUser", signUpPath, S.??("Crear usuario"), createUserMenuLocParams)))

  /**
   * The LocParams for the menu item for creating the user/sign up.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  /*protected def createUserMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(signupFunc.map(_()) openOr signup)) ::
    If(notLoggedIn_? _, S.??("logout.first")) ::
    Nil */
    
   protected def createUserMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(signupFunc.map(_()) openOr signup)) ::
    testLogginIn ::
    Nil

  /**
   * The menu item for lost password (make this "Empty" to disable)
   */
    // lost.password
  def lostPasswordMenuLoc: Box[Menu] =
    Full(Menu(Loc("LostPassword", lostPasswordPath, S.??("Contraseña olvidada"), lostPasswordMenuLocParams))) // not logged in  // Olvido su contraseña?
    
  /**
   * The LocParams for the menu item for lost password.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
    // logout.first
  /** protected def lostPasswordMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(lostPassword)) ::
    If(notLoggedIn_? _, S.??("Primero cierre sesion")) ::
    Nil */
  
    protected def lostPasswordMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(lostPassword)) ::
    testLogginIn ::
    Nil
    

  /**
   * The menu item for resetting the password (make this "Empty" to disable)
   */
    // reset.password
  def resetPasswordMenuLoc: Box[Menu] =
    Full(Menu(Loc("ResetPassword", (passwordResetPath, true), S.??("Reiniciar contraseña"), resetPasswordMenuLocParams))) //not Logged in

  /**
   * The LocParams for the menu item for resetting the password.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
    // logout.first
  protected def resetPasswordMenuLocParams: List[LocParam[Unit]] =
    Hidden ::
    Template(() => wrapIt(passwordReset(snarfLastItem))) ::
    If(notLoggedIn_? _, S.??("Primero cerrar sesión")) ::
    Nil

  /**
   * The menu item for editing the user (make this "Empty" to disable)
   */
    // edit.user
  def editUserMenuLoc: Box[Menu] =
    Full(Menu(Loc("EditUser", editPath, S.??("Editar usuario"), editUserMenuLocParams)))

  /**
   * The LocParams for the menu item for editing the user.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def editUserMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(editFunc.map(_()) openOr edit)) ::
    testLogginIn ::
    Nil

  /**
   * The menu item for changing password (make this "Empty" to disable)
   */
    // change.password
  def changePasswordMenuLoc: Box[Menu] =
    Full(Menu(Loc("ChangePassword", changePasswordPath, S.??("Cambiar contraseña"), changePasswordMenuLocParams)))

  /**
   * The LocParams for the menu item for changing password.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def changePasswordMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(changePassword)) ::
    testLogginIn ::
    Nil

  /**
   * The menu item for validating a user (make this "Empty" to disable)
   */
    // validate.user
  def validateUserMenuLoc: Box[Menu] =
    Full(Menu(Loc("ValidateUser", (validateUserPath, true), S.??("Validar usuario"), validateUserMenuLocParams)))

  /**
   * The LocParams for the menu item for validating a user.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
    // logout.first
  protected def validateUserMenuLocParams: List[LocParam[Unit]] =
    Hidden ::
    Template(() => wrapIt(validateUser(snarfLastItem))) ::
    If(notLoggedIn_? _, S.??("Primero cerrar sesión")) ::
    Nil

/**
* An alias for the sitemap property
*/
def menus: List[Menu] = sitemap // issue 182

  lazy val sitemap: List[Menu] =
  List(homeMenuLoc,loginMenuLoc, logoutMenuLoc, 
       changePasswordMenuLoc,
       validateUserMenuLoc).flatten(a => a)
       
       
/* 
 * 27/09/2010
 * lazy val sitemap: List[Menu] =
  List(homeMenuLoc,loginMenuLoc, logoutMenuLoc, 
       lostPasswordMenuLoc, resetPasswordMenuLoc,
       editUserMenuLoc, changePasswordMenuLoc,
       validateUserMenuLoc).flatten(a => a)
 * 
 * List(loginMenuLoc, logoutMenuLoc, createUserMenuLoc,
       lostPasswordMenuLoc, resetPasswordMenuLoc,
       editUserMenuLoc, changePasswordMenuLoc,
       validateUserMenuLoc).flatten(a => a) */       

  def skipEmailValidation = false

  def userMenu: List[Node] = {
    val li = loggedIn_?
    ItemList.
    filter(i => i.display && i.loggedIn == li).
    map(i => (<a href={i.pathStr}>{i.name}</a>))
  }

  protected def snarfLastItem: String =
  (for (r <- S.request) yield r.path.wholePath.last) openOr ""

  lazy val ItemList: List[MenuItem] =
  List(MenuItem(S.??("Registro"), signUpPath, false),   // Registrarse
       MenuItem(S.??("Ingresar"), loginPath, false),
       MenuItem(S.??("Contraseña  olvidada"), lostPasswordPath, false),
       MenuItem("", passwordResetPath, false),
       MenuItem(S.??("Cambiar contraseña"), changePasswordPath, true),
       MenuItem(S.??("Salir"), logoutPath, true),
       MenuItem(S.??("Editar usuario"), editPath, true),
       MenuItem("", validateUserPath, false))
       
   /*
    * List(MenuItem(S.??("sign.up"), signUpPath, false),
       MenuItem(S.??("log.in"), loginPath, false),
       MenuItem(S.??("lost.password"), lostPasswordPath, false),
       MenuItem("", passwordResetPath, false),
       MenuItem(S.??("change.password"), changePasswordPath, true),
       MenuItem(S.??("log.out"), logoutPath, true),
       MenuItem(S.??("edit.profile"), editPath, true),
       MenuItem("", validateUserPath, false))*/    

  // def requestLoans: List[LoanWrapper] = Nil // List(curUser)

  var onLogIn: List[ModelType => Unit] = Nil

  var onLogOut: List[Box[ModelType] => Unit] = Nil

  /**
   * This function is given a chance to log in a user
   * programmatically when needed
   */
  var autologinFunc: Box[()=>Unit] = Empty

  //def loggedIn_? : Boolean = currentUserId.isDefined
  def loggedIn_? = {
    if(!currentUserId.isDefined)
      for(f <- autologinFunc) f()
    currentUserId.isDefined
  }

  def logUserIdIn(id: String) {
    curUser.remove()
    curUserId(Full(id))
    // Agregado para la aplicacion
    Aplicacion.logApplicationIdIn(CommonsAplicacionActual.idAplicacionActual.toString)
  }
  def logUserIn(who: ModelType) {
    curUser.remove()
    curUserId(Full(who.id.toString))
    onLogIn.foreach(_(who))
    // Agregado para la aplicacion
    Aplicacion.logApplicationIdIn(CommonsAplicacionActual.idAplicacionActual.toString)
  }

  def logoutCurrentUser = logUserOut()

  def logUserOut() {
    onLogOut.foreach(_(curUser))
    curUserId.remove()
    curUser.remove()
    S.request.foreach(_.request.session.terminate)
  }

  private object curUserId extends SessionVar[Box[String]](Empty)

  def currentUserId: Box[String] = curUserId.is

  private object curUser extends RequestVar[Box[ModelType]](currentUserId.flatMap(id => getSingleton.find(id)))  with CleanRequestVarOnSessionTransition


  def currentUser: Box[ModelType] = curUser.is

  def signupXhtml(user: ModelType) = {
    (<form method="post" action={S.uri}><table><tr><td
              colspan="2">{ S.??("Registro") }</td></tr>
          {localForm(user, false)}
          <tr><td>&nbsp;</td><td><user:submit/></td></tr>
                                        </table></form>)
  }


  def signupMailBody(user: ModelType, validationLink: String) = {
    (<html>
        <head>
          <title>{S.??("Confirmación de registro")}</title>
        </head>
        <body>
          <p>{S.??("Estimad@")} {user.firstName},
            <br/>
            <br/>
            {S.??("Enlace de validación de registro")}
            <br/><a href={validationLink}>{validationLink}</a>
            <br/>
            <br/>
            {S.??("Gracias")}
          </p>
        </body>
     </html>)
  }

  def signupMailSubject = S.??("Confirmación de registro")

  def sendValidationEmail(user: ModelType) {
    val resetLink = S.hostAndPath+"/"+validateUserPath.mkString("/")+
    "/"+user.uniqueId

    val email: String = user.email

    val msgXml = signupMailBody(user, resetLink)

    Mailer.sendMail(From(emailFrom),Subject(signupMailSubject),
                    (To(user.email) :: xmlToMailBodyType(msgXml) ::
                     (bccEmail.toList.map(BCC(_)))) :_* )
  }

  protected object signupFunc extends RequestVar[Box[() => NodeSeq]](Empty)

  /**
   * Override this method to do something else after the user signs up
   */
  protected def actionsAfterSignup(theUser: ModelType) {
    theUser.validated(skipEmailValidation).uniqueId.reset()
    theUser.save
    if (!skipEmailValidation) {
      sendValidationEmail(theUser)
      S.notice(S.??("Mensaje de registro"))   //sign.up.message
    } else {
      // S.notice(S.??("Bienvenido"))
      // logUserIn(theUser)
	S.notice(S.??("Usuario creado exitosamente"))
    }
  }

  /**
   * Override this method to validate the user signup (eg by adding captcha verification)
   */
  def validateSignup(user: ModelType): List[FieldError] = user.validate
  
  def signup = {
    val theUser: ModelType = create
    val theName = signUpPath.mkString("")

    def testSignup() {
      validateSignup(theUser) match {
        case Nil =>
          actionsAfterSignup(theUser)
          S.redirectTo(homePage)

        case xs => S.error(xs) ; signupFunc(Full(innerSignup _))
      }
    }

    def innerSignup = bind("user",
                           signupXhtml(theUser),
                           "submit" -> SHtml.submit(S.??("Registrar"), testSignup _))

    innerSignup
  }

  def emailFrom = "noreply@"+S.hostName

  def bccEmail: Box[String] = Empty

  def testLoggedIn(page: String): Boolean =
  ItemList.filter(_.endOfPath == page) match {
    case x :: xs if x.loggedIn == loggedIn_? => true
    case _ => false
  }


  def validateUser(id: String): NodeSeq = getSingleton.find(By(uniqueId, id)) match {
    case Full(user) if !user.validated =>
      user.validated(true).uniqueId.reset().save
      S.notice(S.??("Cuenta validada"))
      logUserIn(user)
      S.redirectTo(homePage)

    case _ => S.error(S.??("Enlace de validación: no válido")); S.redirectTo(homePage)
  }

  def loginXhtml = {
    (<form method="post" action={S.uri}><table><tr><td
              colspan="2"> <h3>{S.??("Ingresar")} </h3> </td></tr>
          <tr><td>{S.??("Correo electrónico")}</td><td><user:email /></td></tr>
          <tr><td>{S.??("Contraseña")}</td><td><user:password /></td></tr>
          <tr><td></td><td><user:submit /></td></tr>
          </table>
     </form>)
  }
  
  //  <tr><td><a href={lostPasswordPath.mkString("/", "/", "")}
  //   >{S.??("Recuperar contraseña")}</a></td><td><user:submit /></td></tr>

  def login = {
    if (S.post_?) {
      S.param("username").
      flatMap(username => getSingleton.find(By(email, username))) match {
        case Full(user) if user.validated &&
          user.password.match_?(S.param("password").openOr("*")) =>
          S.notice(S.??("Conectado"))
          logUserIn(user)
          //S.redirectTo(homePage)
          val redir = loginRedirect.is match {
            case Full(url) =>
              loginRedirect(Empty)
              url
            case _ =>
              homePage
          }
          S.redirectTo(redir)

        case Full(user) if !user.validated =>
          S.error(S.??("Error en validación de cuenta"))

        case _ => S.error(S.??("Credenciales no válidas"))
      }
    }

    bind("user", loginXhtml,
         "email" -> (FocusOnLoad(<input type="text" name="username"/>)),
         "password" -> (<input type="password" name="password"/>),
         "submit" -> (<input type="submit" value={S.??("Ingresar")}/>))
  }

  def lostPasswordXhtml = {
    (<form method="post" action={S.uri}>
        <table><tr><td
              colspan="2">{S.??("Ingresar correo electrónico")}</td></tr>
          <tr><td>{S.??("Dirección de correo electrónico")}</td><td><user:email /></td></tr>
          <tr><td>&nbsp;</td><td><user:submit /></td></tr>
        </table>
     </form>)
  }

  def passwordResetMailBody(user: ModelType, resetLink: String) = {
    (<html>
        <head>
          <title>{S.??("Confirmación de reinicio de contraseña")}</title>
        </head>
        <body>
          <p>{S.??("Estimad@")} {user.firstName},
            <br/>
            <br/>
            {S.??("Haga click en el vínculo de reinicio")}
            <br/><a href={resetLink}>{resetLink}</a>
            <br/>
            <br/>
            {S.??("Gracias")}
          </p>
        </body>
     </html>)
  }

  def passwordResetEmailSubject = S.??("solicitud de restablecimiento de contraseña")

  def sendPasswordReset(email: String) {
    getSingleton.find(By(this.email, email)) match {
      case Full(user) if user.validated =>
        user.uniqueId.reset().save
        val resetLink = S.hostAndPath+
        passwordResetPath.mkString("/", "/", "/")+user.uniqueId

        val email: String = user.email

        val msgXml = passwordResetMailBody(user, resetLink)
        Mailer.sendMail(From(emailFrom),Subject(passwordResetEmailSubject),
                        (To(user.email) :: xmlToMailBodyType(msgXml) ::
                         (bccEmail.toList.map(BCC(_)))) :_*)

        S.notice(S.??("Restablecimiento de contraseña enviado"))
        S.redirectTo(homePage)

      case Full(user) =>
        sendValidationEmail(user)
        S.notice(S.??("Validación de cuenta reenviada"))  // account.validation.resent
        S.redirectTo(homePage)

      case _ => S.error(S.??("Dirección de correo electrónico no encontrado"))  // email.address.not.found
    }
  }

  def lostPassword = {
    bind("user", lostPasswordXhtml,
         "email" -> SHtml.text("", sendPasswordReset _),
         "submit" -> <input type="submit" value={S.??("Enviar")} />)  // send.it
  }

  def passwordResetXhtml = {
    (<form method="post" action={S.uri}>
        <table><tr><td colspan="2">{S.??("Reiniciar contraseña")}</td></tr>
          <tr><td>{S.??("Ingresar nueva contraseña")}</td><td><user:pwd/></td></tr>
          <tr><td>{S.??("Repetir nueva contraseña")}</td><td><user:pwd/></td></tr>
          <tr><td>&nbsp;</td><td><user:submit/></td></tr>
        </table>
     </form>)
  }

  def passwordReset(id: String) =
  getSingleton.find(By(uniqueId, id)) match {
    case Full(user) =>
      def finishSet() {
        user.validate match {
          case Nil => S.notice(S.??("Contraseña modificada"))  // password.changed
            user.save
            logUserIn(user); S.redirectTo(homePage)

          case xs => S.error(xs)
        }
      }
      user.uniqueId.reset().save

      bind("user", passwordResetXhtml,
           "pwd" -> SHtml.password_*("",(p: List[String]) =>
          user.password.setList(p)),
           "submit" -> SHtml.submit(S.??("Contraseña definida"), finishSet _))   // set.password
    case _ => S.error(S.??("Enlace de contraseña no válido")); S.redirectTo(homePage)
  }

  def changePasswordXhtml = {
    (<form method="post" action={S.uri}>
        <table><tr><td colspan="2"> <h3>{S.??("Cambiar contraseña")} </h3> </td></tr>
          <tr><td>{S.??("Antigua contraseña")}</td><td><user:old_pwd /></td></tr>
          <tr><td>{S.??("Nueva contraseña")}</td><td><user:new_pwd /></td></tr>
          <tr><td>{S.??("Repetir contraseña")}</td><td><user:new_pwd /></td></tr>
          <tr><td>&nbsp;</td><td><user:submit /></td></tr>
        </table>
     </form>)
  }

  def changePassword = {
    val user = currentUser.open_! // we can do this because the logged in test has happened
    var oldPassword = ""
    var newPassword: List[String] = Nil

    def testAndSet() {
      if (!user.password.match_?(oldPassword)) S.error(S.??("Antigua contraseña incorrecta"))
      else {
        user.password.setFromAny(newPassword)
        user.validate match {
          case Nil => user.save; S.notice(S.??("Contraseña modificada")); S.redirectTo(homePage)
          case xs => S.error(xs)
        }
      }
    }

    bind("user", changePasswordXhtml,
         "old_pwd" -> SHtml.password("", s => oldPassword = s),
         "new_pwd" -> SHtml.password_*("", LFuncHolder(s => newPassword = s)),
         "submit" -> SHtml.submit(S.??("Cambiar"), testAndSet _))
  }

  def editXhtml(user: ModelType) = {
    (<form method="post" action={S.uri}>
        <table><tr><td colspan="2">{S.??("Editar")}</td></tr>
          {localForm(user, true)}
          <tr><td>&nbsp;</td><td><user:submit/></td></tr>
        </table>
     </form>)
  }

  object editFunc extends RequestVar[Box[() => NodeSeq]](Empty)

  def edit() = {
    val theUser: ModelType = currentUser.open_! // we know we're logged in
    val theName = editPath.mkString("")

    def testEdit() {
      theUser.validate match {
        case Nil =>
          theUser.save
          S.notice(S.??("Perfil actualizado"))  // profile.updated
          S.redirectTo(homePage)

        case xs => S.error(xs) ; editFunc(Full(innerEdit _))
      }
    }

    def innerEdit = bind("user", editXhtml(theUser),
                         "submit" -> SHtml.submit(S.??("Editar"), testEdit _))

    innerEdit
  }

  def logout = {
    logoutCurrentUser
    S.redirectTo(homePage)
  }

  protected def localForm(user: ModelType, ignorePassword: Boolean): NodeSeq = {
    signupFields.
    map(fi => getSingleton.getActualBaseField(user, fi)).
    filter(f => !ignorePassword || (f match {
          case f: MappedPassword[ModelType] => false
          case _ => true
        })).
    flatMap(f =>
      f.toForm.toList.map(form =>
        (<tr><td>{f.displayName}</td><td>{form}</td></tr>) ) )
  }

  protected def wrapIt(in: NodeSeq): NodeSeq =
  screenWrap.map(new RuleTransformer(new RewriteRule {
        override def transform(n: Node) = n match {
          case e: Elem if "bind" == e.label && "lift" == e.prefix => in
          case _ => n
        }
      })) openOr in
}

trait MegaProtoUserPerfil[T <: MegaProtoUserPerfil[T]] extends ProtoUserPerfil[T] {
  self: T =>
  object uniqueId extends MappedUniqueId(this, 32) {
    override def dbIndexed_? = true
    override def writePermission_?  = true
  }

  object validated extends MappedBoolean[T](this) {
    override def defaultValue = false
    override val fieldId = Some(Text("txtValidated"))
  }

  object locale extends MappedLocale[T](this) {
    override def displayName = fieldOwner.localeDisplayName
    override val fieldId = Some(Text("txtLocale"))
  }
  
 /* 
  object perfil_Id extends MappedPerfil[T](this) {
		override def displayName = fieldOwner.perfilIdDisplayName
		override val fieldId = Some(Text("txtPerfilId"))
  } */
  
  object unidad_Id extends MappedLongForeignKey(this, Unidad) {
		override def displayName = fieldOwner.unidadIdDisplayName
		override val fieldId = Some(Text("txtUnidadId"))
		
		def _toForm(editable:Boolean) : Box[Elem] =
		if (editable) {	
			Full(SHtml.select(Unidad.findAll(NotBy(Unidad.codigo, CommonsUnidad.patronUnidadTraspaso), By(Unidad.codAccesoOper, CommonsUnidad.codAccesoOperDirecta)).  
			toList.sort(_.getDisplayName < _.getDisplayName).  
			map(lo => (lo.id.toString, lo.getDisplayName)),  
			Full(this.is.toString), setUnidadId)  )		
		} else {	
			Full(SHtml.select(Unidad.findAll(NotBy(Unidad.codigo, CommonsUnidad.patronUnidadTraspaso)).  
			toList.sort(_.getDisplayName < _.getDisplayName).  
			map(lo => (lo.id.toString, lo.getDisplayName)),  
			Full(this.is.toString), setUnidadId) % ("disabled" -> "disabled")  )		
		} 
		
		def setUnidadId(id:String) = {
        	this.set(id.toLong) 
        }

  }
  
  object activo extends MappedBoolean[T](this) {
		override def displayName = fieldOwner.activoDisplayName
		override val fieldId = Some(Text("txtActivo"))
  }

  object timezone extends MappedTimeZone[T](this) {
    override def displayName = fieldOwner.timezoneDisplayName
    override val fieldId = Some(Text("txtTimeZone"))
  }

  def timezoneDisplayName = ??("Huso Horario") // time.zone
  def localeDisplayName = ??("Localización")  // locale
  // def perfilIdDisplayName = ??("Código de Perfil")  // locale
  def activoDisplayName = ??("Activo")  // locale
  def unidadIdDisplayName = ??("Código de unidad")
  
}


// Definicion de User
/**
 * The singleton that has methods for accessing the database
 */
object Usuario extends Usuario with MetaMegaProtoUserPerfil[Usuario] {
  	
  override def screenWrap = Full(<lift:surround with="default" at="content">
			       <lift:bind /></lift:surround>)
  // define the order fields will appear in forms and output
  // override def fieldOrder = List(id, firstName, lastName, email,
  // locale, timezone, password, perfil_Id, textArea)

  override def fieldOrder = List(id, firstName, lastName, email,
  locale, timezone, unidad_Id, password, textArea)
  
  // comment this line out to require email validations
  override def skipEmailValidation = true       
}	

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class Usuario extends MegaProtoUserPerfil[Usuario] {
  def getSingleton = Usuario // what's the "meta" server

  // define an additional field for a personal essay
  object textArea extends MappedTextarea(this, 2048) {
    override def textareaRows  = 10
    override def textareaCols = 50
  }  
}