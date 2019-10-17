with Concorde.UI.Models.Login;

package Concorde.UI.Views.Login is

   function Login_View
     (Model : not null access Concorde.UI.Models.Login.Root_Login_Model'Class)
      return View_Type;

end Concorde.UI.Views.Login;
