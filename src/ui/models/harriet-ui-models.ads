private with Harriet.Db.Orbiting_Object;

with Harriet.Db;
with Harriet.Json;

package Harriet.UI.Models is

   type Root_Harriet_Model is abstract tagged private;

   type Harriet_Model is access all Root_Harriet_Model'Class;

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

   function Changed
     (Model : Root_Harriet_Model)
      return Boolean
      is abstract;

   procedure Update
     (Model : in out Root_Harriet_Model)
   is null;

   function Handle
     (Model   : in out Root_Harriet_Model;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
   is abstract;

   function Get
     (Model   : Root_Harriet_Model;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
      is abstract;

   function Error
     (Model   : Root_Harriet_Model'class;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class;
      Message : String)
      return Harriet.Json.Json_Value'Class;

   procedure Close
     (Model : in out Harriet_Model);

private

   type Root_Harriet_Model is abstract tagged
      record
         null;
      end record;

   function Serialize
     (Object : Harriet.Db.Orbiting_Object.Orbiting_Object_Type;
      Full   : Boolean)
      return Json.Json_Value'Class;

end Harriet.UI.Models;
