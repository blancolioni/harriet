with Concorde.UI.Models.Tables;

package Concorde.UI.Views.Tables is

   function Create_Table_View
     (Model         : not null access
        Concorde.UI.Models.Tables.Root_Table_Model'Class;
      Headings_Down : Boolean := False)
      return View_Type;

end Concorde.UI.Views.Tables;
