with Harriet.Color;
with Harriet.Constants;
with Harriet.Solar_System;

with Harriet.Factions;
with Harriet.Worlds;

with Harriet.Db.Atmosphere;
with Harriet.Db.Colony;
with Harriet.Db.Faction;
with Harriet.Db.Gas;
with Harriet.Db.Palette_Entry;
with Harriet.Db.Ship;
with Harriet.Db.World;

package body Harriet.UI.Models.Worlds is

   type World_Model_Type is
     new Harriet.UI.Models.Root_Harriet_Model with
      record
         World  : Harriet.Db.World_Reference :=
           Harriet.Db.Null_World_Reference;
         Colony : Harriet.Db.Colony_Reference :=
           Harriet.Db.Null_Colony_Reference;
      end record;

   overriding function Handle
     (Model   : in out World_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class;

   overriding procedure Start
     (Model     : in out World_Model_Type;
      User      : Harriet.Db.User_Reference;
      Arguments : String);

   overriding function Name
     (Model : World_Model_Type)
      return String
   is ("world");

   overriding function Default_View_Name
     (Model : World_Model_Type)
      return String
   is ("World");

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Model   : in out World_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (State, Client, Request);
      World : constant Harriet.Db.World.World_Type :=
        Harriet.Db.World.Get (Model.World);

      Data  : Json.Json_Object;

      procedure Prop
        (Name : String;
         Value : Real);

      procedure Prop
        (Name  : String;
         Value : String);

      ----------
      -- Prop --
      ----------

      procedure Prop
        (Name  : String;
         Value : Real)
      is
      begin
         Data.Set_Property (Name, Json.Float_Value (Float (Value)));
      end Prop;

      ----------
      -- Prop --
      ----------

      procedure Prop
        (Name  : String;
         Value : String)
      is
      begin
         Data.Set_Property (Name, Value);
      end Prop;

      use Harriet.Solar_System;

   begin
      Prop ("name", World.Name);
      Prop ("category", Harriet.Db.World_Category'Image (World.Category));
      Prop ("mass", World.Mass / Earth_Mass);
      Prop ("radius", World.Radius / Earth_Radius);
      Prop ("density", World.Density / Earth_Density);
      Prop ("orbit", World.Semimajor_Axis / Earth_Orbit);
      Prop ("day", World.Rotation_Period / 3600.0);
      Prop ("tilt", World.Tilt);
      Prop ("gravity", World.Surface_Gravity / Earth_Gravity);
      Prop ("surfaceTemperature",
            World.Surface_Temperature
            - Harriet.Constants.Freezing_Point_Of_Water);

      declare
         Atmosphere : Json.Json_Array;
      begin
         for Atm of Harriet.Db.Atmosphere.Select_By_World (Model.World) loop
            declare
               Gas : constant Harriet.Db.Gas.Gas_Type :=
                 Harriet.Db.Gas.Get (Atm.Gas);
               Gas_Object : Json.Json_Object;
            begin
               Gas_Object.Set_Property ("name", Gas.Name);
               Gas_Object.Set_Property ("formula", Gas.Formula);
               Gas_Object.Set_Property
                 ("fraction", Json.Float_Value (Float (Atm.Percentage)));
               Atmosphere.Append (Gas_Object);
            end;
         end loop;
         Data.Set_Property ("atmosphere", Atmosphere);
      end;

      declare
         Palette : Json.Json_Array;
      begin
         for Item of
           Harriet.Db.Palette_Entry.Select_By_Palette (World.Palette)
         loop
            declare
               Palette_Object : Json.Json_Object;
            begin
               Palette_Object.Set_Property
                 ("r", Json.Float_Value (Float (Item.Red)));
               Palette_Object.Set_Property
                 ("g", Json.Float_Value (Float (Item.Green)));
               Palette_Object.Set_Property
                 ("b", Json.Float_Value (Float (Item.Blue)));
               Palette.Append (Palette_Object);
            end;
         end loop;
         Data.Set_Property ("palette", Palette);
      end;

      declare
         Ships : Json.Json_Array;
      begin
         for Ship of Harriet.Db.Ship.Select_By_World (Model.World) loop
            declare
               Faction     : constant Harriet.Db.Faction.Faction_Type :=
                 Harriet.Db.Faction.Get (Ship.Faction);
               Ship_Object : Json.Json_Object;
            begin
               Ship_Object.Set_Property ("name", Ship.Name);
               Ship_Object.Set_Property
                 ("owner", Faction.Name);
               Ship_Object.Set_Property
                 ("color",
                  Harriet.Color.To_Html_String
                    (Faction.Red, Faction.Green, Faction.Blue));
               Ship_Object.Set_Property
                 ("orbit", Float (Ship.Orbit / World.Radius));
               Ship_Object.Set_Property
                 ("inclination", 0.0);
               Ships.Append (Ship_Object);
            end;
         end loop;
         Data.Set_Property ("ships", Ships);
      end;

      return Result : Json.Json_Object do
         Result.Set_Property ("world", Data);
      end return;

   end Handle;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Model     : in out World_Model_Type;
      User      : Harriet.Db.User_Reference;
      Arguments : String)
   is
      use Harriet.Db;
      Name : constant String :=
        (if Arguments = ""
         then Harriet.Worlds.Name
           (Harriet.Factions.Get_User_Faction (User).Capital_World)
         else Arguments);
      World : constant Harriet.Db.World_Reference :=
        Harriet.Db.World.First_Reference_By_Name (Name);
      Colony : constant Harriet.Db.Colony_Reference :=
        (if World = Null_World_Reference then Null_Colony_Reference
         else Harriet.Db.Colony.Get_Reference_By_World (World));

   begin
      Model.World := World;
      Model.Colony := Colony;
   end Start;

   -----------------
   -- World_Model --
   -----------------

   function World_Model return Root_Harriet_Model'Class is
   begin
      return Model : World_Model_Type;
   end World_Model;

end Harriet.UI.Models.Worlds;
