with Concorde.UI.Models.Dashboard;

package Concorde.UI.Views.Dashboard is

   function Dashboard_View
     (Model : not null access
        Concorde.UI.Models.Dashboard.Root_Dashboard_Model'Class)
      return View_Type;

end Concorde.UI.Views.Dashboard;
