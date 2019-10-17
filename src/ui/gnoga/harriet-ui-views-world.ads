with Concorde.UI.Models.World;

package Concorde.UI.Views.World is

   function World_View
     (Model : not null access
        Concorde.UI.Models.World.Root_World_Model'Class)
      return View_Type;

end Concorde.UI.Views.World;
