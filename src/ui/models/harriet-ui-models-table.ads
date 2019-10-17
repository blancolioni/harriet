package Harriet.UI.Models.Table is

   type Root_Table_Model is new Root_Harriet_Model with private;

   overriding function Handle
     (Model   : in out Root_Table_Model;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class;

private

   type Root_Table_Model is new Root_Harriet_Model with
      record
         null;
      end record;

end Harriet.UI.Models.Table;
