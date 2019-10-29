with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Harriet.UI.Models is

   -----------
   -- Close --
   -----------

   procedure Close
     (Model : in out Harriet_Model)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Root_Harriet_Model'Class, Harriet_Model);
   begin
      Free (Model);
      Model := null;
   end Close;

   -----------
   -- Error --
   -----------

   function Error
     (Model   : Root_Harriet_Model'class;
      State   : State_Interface'Class;
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
