with Harriet.Factions;

with Harriet.UI.Models.Data_Source;

with Harriet.Db.Star;
with Harriet.Db.Star_System;

package body Harriet.UI.Models.Star_System is

   type Star_System_Model_Type is
     new Root_Harriet_Model with
      record
         Star_System : Harriet.Db.Star_System_Reference :=
           Harriet.Db.Null_Star_System_Reference;
      end record;

   overriding procedure Start
     (Model     : in out Star_System_Model_Type;
      User      : Harriet.Db.User_Reference;
      Arguments : String);

   overriding function Name
     (Model : Star_System_Model_Type)
      return String
   is ("star-system");

   overriding function Default_View_Name
     (Model : Star_System_Model_Type)
      return String
   is ("System");

   overriding function Changed
     (Model : Star_System_Model_Type)
      return Boolean;

   overriding function Handle
     (Model   : in out Star_System_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class;

   overriding function Get
     (Model   : Star_System_Model_Type;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class;

   -------------
   -- Changed --
   -------------

   overriding function Changed
     (Model : Star_System_Model_Type)
      return Boolean
   is
      pragma Unreferenced (Model);
   begin
      return True;
   end Changed;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Model   : Star_System_Model_Type;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (State, Client);

      Result : Json.Json_Object;

   begin
      Result.Set_Property
        ("title", Harriet.Db.Star_System.Get (Model.Star_System).Name);
      Result.Set_Property
        ("systemName", Harriet.Db.Star_System.Get (Model.Star_System).Name);
      Result.Set_Property
        ("primary",
         Serialize
           (Harriet.Db.Star.First_By_Star_System (Model.Star_System),
            Full => Request.Image /= "changes"));
      return Result;

   end Get;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Model   : in out Star_System_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
   is
   begin
      return Model.Get (State, Client, Request);
   end Handle;

   -----------------------
   -- Star_System_Model --
   -----------------------

   function Star_System_Model return Root_Harriet_Model'Class is
   begin
      return Model : Star_System_Model_Type;
   end Star_System_Model;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Model     : in out Star_System_Model_Type;
      User      : Harriet.Db.User_Reference;
      Arguments : String)
   is
      Faction  : constant Harriet.Factions.Faction_Type'Class :=
        Harriet.Factions.Get_User_Faction (User);

   begin
      if Arguments = "" then
         Model.Star_System := Faction.Capital_System;
      else
         Model.Star_System :=
           Harriet.Db.Star_System.First_Reference_By_Name (Arguments);
      end if;
   end Start;

end Harriet.UI.Models.Star_System;
