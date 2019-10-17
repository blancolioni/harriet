with Harriet.UI.Web_UI;

package body Harriet.UI.Launch is

   ------------
   -- Get_UI --
   ------------

   function Get_UI (Name : String) return UI_Interface'Class is
      pragma Unreferenced (Name);
   begin
      return Harriet.UI.Web_UI.Get_Web_UI;
   end Get_UI;

end Harriet.UI.Launch;
