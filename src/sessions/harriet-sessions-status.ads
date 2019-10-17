private package Harriet.Sessions.Status is

   function Get_Update_Speed
     (Session : Root_Harriet_Session'Class)
      return Json.Json_Value'Class;

   procedure Set_Update_Speed
     (Session : in out Root_Harriet_Session'Class;
      Value   : Json.Json_Value'Class);

end Harriet.Sessions.Status;
