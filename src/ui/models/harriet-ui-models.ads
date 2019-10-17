with Harriet.Db;
with Harriet.Json;

package Harriet.UI.Models is

   type Root_Harriet_Model is abstract tagged private;

   function Name (Model : Root_Harriet_Model) return String
                  is abstract;

   function Default_View_Name
     (Model : Root_Harriet_Model)
      return String
      is abstract;

   procedure Start
     (Model     : in out Root_Harriet_Model;
      User      : Harriet.Db.User_Reference;
      Arguments : String)
   is null;

   function Handle
     (Model   : in out Root_Harriet_Model;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
   is abstract;

   function Error
     (Model   : in out Root_Harriet_Model'class;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class;
      Message : String)
      return Harriet.Json.Json_Value'Class;

private

   type Root_Harriet_Model is abstract tagged
      record
         null;
      end record;

end Harriet.UI.Models;
