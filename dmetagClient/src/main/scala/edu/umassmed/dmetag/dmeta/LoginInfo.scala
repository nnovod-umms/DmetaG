package edu.umassmed.dmetag.dmeta

import com.raquo.laminar.api._
import edu.umassmed.dmetag.dmeta.DMeta.Login
import DMeta.{Login, LoginData, LoginUser}

/**
 * Keep track of login done
 */
object LoginInfo {
  // Remember login (none at start)
  val LoginResponse: L.Var[Option[Login]] = L.Var[Option[Login]](None)

  // Failed login in case an empty one is needed
  val EmptyLogin: Login = Login(status = "failed", token = "badtoken",
    data = LoginData(LoginUser(photo = "None", role = "None", email = "None",
      name = "None", username = "None", scope = "None", sso_id = "None")))

  // Is anyone logged in?
  def isLoggedIn: Boolean = LoginResponse.now().nonEmpty
}
