with Concorde.UI.Models.Galaxy;

package Concorde.UI.Views.Galaxy is

   function Galaxy_View
     (Model : not null access
        Concorde.UI.Models.Galaxy.Root_Galaxy_Model'Class)
      return View_Type;

end Concorde.UI.Views.Galaxy;
