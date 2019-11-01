with Ada.Exceptions;

with Harriet.Calendar;
with Harriet.Color;
with Harriet.Orbits;
with Harriet.Solar_System;

with Harriet.Factions;
with Harriet.Stars;

with Harriet.UI.Models.Data_Source;

with Harriet.Db.Massive_Object;
with Harriet.Db.Star;
with Harriet.Db.Star_System;
with Harriet.Db.Star_System_Object;
with Harriet.Db.World;

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
      pragma Unreferenced (State, Client, Request);

      function Orbiting_Objects
        (Primary : Harriet.Db.Star_System_Object_Reference)
         return Json.Json_Array;

      function To_Json
        (Object : Harriet.Db.Star_System_Object.Star_System_Object_Type)
         return Json.Json_Object;

      ----------------------
      -- Orbiting_Objects --
      ----------------------

      function Orbiting_Objects
        (Primary : Harriet.Db.Star_System_Object_Reference)
         return Json.Json_Array
      is
      begin
         return Arr : Json.Json_Array do
            for Item of
              Harriet.Db.Star_System_Object.Select_By_Primary
                (Primary)
            loop
               Arr.Append (To_Json (Item));
            end loop;
         end return;
      end Orbiting_Objects;

      -------------
      -- To_Json --
      -------------

      function To_Json
        (Object : Harriet.Db.Star_System_Object.Star_System_Object_Type)
         return Json.Json_Object
      is
         use Harriet.Calendar;
         use Harriet.Db;

         Result : Json.Json_Object;

         procedure Set (Property_Name, Property_Value : String);
         procedure Set (Property_Name : String;
                        Property_Value : Real);
         procedure Set (Property_Name  : String;
                        Property_Value : Json.Json_Value'Class);

         ---------
         -- Set --
         ---------

         procedure Set (Property_Name, Property_Value : String) is
         begin
            Result.Set_Property (Property_Name, Property_Value);
         end Set;

         ---------
         -- Set --
         ---------

         procedure Set (Property_Name  : String;
                        Property_Value : Real)
         is
         begin
            Result.Set_Property (Property_Name, Float (Property_Value));
         end Set;

         ---------
         -- Set --
         ---------

         procedure Set (Property_Name  : String;
                        Property_Value : Json.Json_Value'Class)
         is
         begin
            Result.Set_Property (Property_Name, Property_Value);
         end Set;

      begin

         Set ("name", Object.Name);
         Set ("orbit",
              Object.Semimajor_Axis / Harriet.Solar_System.Earth_Orbit);
         Set ("longitude",
              (if Object.Primary_Massive = Null_Massive_Object_Reference
               then 0.0
               else Harriet.Orbits.Calculate_Longitude
                 (Massive_Object.Get (Object.Primary_Massive).Mass,
                  Object.Semimajor_Axis,
                  Harriet.Calendar.Clock - Object.Epoch)));
         Set ("day", Object.Period);
         Set ("axisTilt", Object.Tilt);
         Set ("dependents",
              Orbiting_Objects (Object.Get_Star_System_Object_Reference));

         if Object.Top_Record = R_Star then
            declare
               Star : constant Harriet.Stars.Star_Type'Class :=
                 Harriet.Stars.Get
                   (Harriet.Db.Star.Get_Star
                      (Object.Get_Star_System_Object_Reference));
               Color : constant Harriet.Color.Harriet_Color :=
                 Star.Color;
            begin
               Set ("mass", Object.Mass / Harriet.Solar_System.Solar_Mass);
               Set ("radius",
                    Object.Radius / Harriet.Solar_System.Solar_Radius);
               Set ("temperature", Star.Temperature);
               Set ("red", Color.Red);
               Set ("green", Color.Green);
               Set ("blue", Color.Blue);
            end;
         elsif Object.Top_Record = R_World then
            declare
               World  : constant Harriet.Db.World.World_Type :=
                 Harriet.Db.World.Get_World
                   (Object.Get_Star_System_Object_Reference);
            begin
               Set ("mass", Object.Mass / Harriet.Solar_System.Earth_Mass);
               Set ("radius",
                    Object.Radius / Harriet.Solar_System.Earth_Radius);
               Set ("temperature", World.Average_Temperature);
               Set ("composition",
                    Harriet.Db.World_Composition'Image
                      (World.Composition));
               Set ("climate",
                    Harriet.Db.World_Climate'Image
                      (World.Climate));
            end;

         end if;

         return Result;

      exception
         when E : others =>
            raise Constraint_Error with
              "exception while serializing "
              & Record_Type'Image (Object.Top_Record)
              & " "
              & Object.Name
              & ": "
              & Ada.Exceptions.Exception_Message (E);
      end To_Json;

      Result : Json.Json_Object;

   begin
      Result.Set_Property
        ("title", Harriet.Db.Star_System.Get (Model.Star_System).Name);
      Result.Set_Property
        ("systemName", Harriet.Db.Star_System.Get (Model.Star_System).Name);
      Result.Set_Property
        ("primary",
         To_Json (Harriet.Db.Star.First_By_Star_System (Model.Star_System)));
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
