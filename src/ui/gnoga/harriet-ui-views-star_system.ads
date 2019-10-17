with Concorde.UI.Models.Star_System;

package Concorde.UI.Views.Star_System is

   function Star_System_View
     (Model : not null access
        Concorde.UI.Models.Star_System.Root_Star_System_Model'Class)
      return View_Type;

end Concorde.UI.Views.Star_System;
