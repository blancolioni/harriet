with Harriet.Orbits;

with Harriet.Factions;
with Harriet.Ships;
with Harriet.Worlds;

with Harriet.UI.Models.Data_Source;
with Harriet.UI.Models.Values;

with Harriet.Db.Ship;
with Harriet.Db.World;

package body Harriet.UI.Models.World_Ships is

   type World_Ship_Model_Type is
     new Harriet.UI.Models.Data_Source.Simple_Data_Source_Model with
      record
         null;
      end record;

   overriding procedure Start
     (Model     : in out World_Ship_Model_Type;
      User      : Harriet.Db.User_Reference;
      Arguments : String);

   overriding function Name
     (Model : World_Ship_Model_Type)
      return String
   is ("world_ship");

   overriding function Default_View_Name
     (Model : World_Ship_Model_Type)
      return String
   is ("Table");

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Model     : in out World_Ship_Model_Type;
      User      : Harriet.Db.User_Reference;
      Arguments : String)
   is
      Faction  : constant Harriet.Factions.Faction_Type'Class :=
        Harriet.Factions.Get_User_Faction (User);

      Capital  : constant Harriet.Db.World_Reference :=
        (if Faction.Has_Element
         then Faction.Capital_World
         else Harriet.Db.Null_World_Reference);

      Name     : constant String :=
        (if Arguments = ""
         then Harriet.Worlds.Name (Capital)
         else Arguments);

      World : constant Harriet.Db.World_Reference :=
        Harriet.Db.World.First_Reference_By_Name (Name);

   begin
      Model.Add_Column
        (Id       => "name",
         Label    => "Name",
         Col_Type => Values.Text_Type);
      Model.Add_Column
        (Id       => "owner",
         Label    => "Owner",
         Col_Type => Values.Text_Type);
      Model.Add_Column
        (Id       => "orbit",
         Label    => "Orbit (km)",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "period",
         Label    => "Period (m)",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "latitude",
         Label    => "Latitude",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "longitude",
         Label    => "Longitude",
         Col_Type => Values.Real_Type);

      declare

         procedure Add_Row
           (Ship : Harriet.Db.Ship.Ship_Type);

         -------------
         -- Add_Row --
         -------------

         procedure Add_Row
           (Ship : Harriet.Db.Ship.Ship_Type)
         is
            function T (S : String) return Values.Model_Value_Type
                        renames Values.Text_Value;
            function R (X : Real) return Values.Model_Value_Type
                        renames Values.Real_Value;

         begin
            Model.Add_Row
              (
                 (T (Ship.Name),
               T (Harriet.Factions.Get (Ship.Faction).Name),
               R ((Ship.Orbit - Harriet.Worlds.Radius (World)) / 1000.0),
               R (Harriet.Orbits.Period
                 (Harriet.Worlds.Mass (World), Ship.Orbit) / 60.0),
               R (0.0),
               R (Harriet.Ships.Current_Longitude
                 (Harriet.Ships.Get (Ship.Get_Ship_Reference)))
              )
              );
         end Add_Row;

      begin
         for Ship of
           Harriet.Db.Ship.Select_By_World (World)
         loop
            Add_Row (Ship);
         end loop;
      end;

   end Start;

   ----------------------
   -- World_Ship_Model --
   ----------------------

   function World_Ship_Model return Root_Harriet_Model'Class is
   begin
      return Model : World_Ship_Model_Type;
   end World_Ship_Model;

end Harriet.UI.Models.World_Ships;
