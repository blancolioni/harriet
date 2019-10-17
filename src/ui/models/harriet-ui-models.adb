with Ada.Text_IO;

package body Harriet.UI.Models is

   -----------
   -- Error --
   -----------

   function Error
     (Model   : in out Root_Harriet_Model'class;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class;
      Message : String)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (State);
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error,
                "error in model " & Model.Name & " client" & Client'Image);
      Put_Line ("request: " & Request.Image);
      Put_Line (Standard_Error,
                "message: " & Message);
      return Response : Harriet.Json.Json_Object do
         Response.Set_Property ("error", Message);
      end return;

   end Error;

end Harriet.UI.Models;
