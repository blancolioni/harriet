with Ada.Text_IO;

with Harriet.Calendar;
with Harriet.Quantities;

with Harriet.Constants;
with Harriet.Solar_System;

with Harriet.Factions;
with Harriet.Star_Systems;

with Harriet.UI.Models.Data_Source;
with Harriet.UI.Models.Values;

with Harriet.Db.Colony;
with Harriet.Db.Star_System;
with Harriet.Db.World;

package body Harriet.UI.Models.Star_System is

   type Star_System_Model_Type is
     new Harriet.UI.Models.Data_Source.Simple_Data_Source_Model with
      record
         Last_Fetch : Harriet.Calendar.Time;
      end record;

   overriding procedure On_Fetched
     (Model : in out Star_System_Model_Type);

   overriding procedure Start
     (Model     : in out Star_System_Model_Type;
      User      : Harriet.Db.User_Reference;
      Arguments : String);

   overriding function Name
     (Model : Star_System_Model_Type)
      return String
   is ("star_system");

   overriding function Default_View_Name
     (Model : Star_System_Model_Type)
      return String
   is ("Table");

   overriding function Changed
     (Model : Star_System_Model_Type)
      return Boolean;

   -------------
   -- Changed --
   -------------

   overriding function Changed
     (Model : Star_System_Model_Type)
      return Boolean
   is
      use type Harriet.Calendar.Time;
   begin
      Ada.Text_IO.Put_Line
        ("star-system: last update "
         & Harriet.Calendar.Image (Model.Last_Fetch, True)
         & "; current time "
         & Harriet.Calendar.Image (Harriet.Calendar.Clock, True));
      return Model.Last_Fetch /= Harriet.Calendar.Clock;
   end Changed;

   ----------------
   -- On_Fetched --
   ----------------

   overriding procedure On_Fetched
     (Model : in out Star_System_Model_Type)
   is
   begin
      Model.Last_Fetch := Harriet.Calendar.Clock;
   end On_Fetched;

   -----------------------
   -- Star_System_Model --
   -----------------------

   function Star_System_Model return Root_Harriet_Model'Class is
   begin
      return Model : constant Star_System_Model_Type :=
        Star_System_Model_Type'
          (Harriet.UI.Models.Data_Source.Simple_Data_Source_Model with
             Last_Fetch => Harriet.Calendar.Clock);
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

      Capital  : constant Harriet.Db.Star_System_Reference :=
        (if Faction.Has_Element
         then Faction.Capital_System
         else Harriet.Db.Null_Star_System_Reference);

      Name     : constant String :=
        (if Arguments = ""
         then Harriet.Star_Systems.Name (Capital)
         else Arguments);

      Star_System : constant Harriet.Db.Star_System_Reference :=
        Harriet.Db.Star_System.First_Reference_By_Name (Name);

   begin
      Model.Add_Column
        (Id       => "name",
         Label    => "Name",
         Col_Type => Values.Text_Type);
      Model.Add_Column
        (Id       => "orbit",
         Label    => "Orbit (AU)",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "radius",
         Label    => "Radius (R"
         & Character'Val (16#E2#)
         & Character'Val (16#82#)
         & Character'Val (16#91#)
         & ")",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id => "gravity",
         Label => "g",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "surfaceTemperature",
         Label    =>
           "Suface Temp ("
         & Character'Val (16#C2#)
         & Character'Val (16#B0#)
         & "C)",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "surfacePressure",
         Label    => "Surface Pressure (atm)",
        Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "category",
         Label    => "Category",
         Col_Type => Values.Text_Type);
      Model.Add_Column
        (Id       => "climate",
         Label    => "Climate",
         Col_Type => Values.Text_Type);
      Model.Add_Column
        (Id       => "population",
         Label    => "Population",
         Col_Type => Values.Real_Type);

      declare

         procedure Add_Row
           (World : Harriet.Db.World.World_Type);

         -------------
         -- Add_Row --
         -------------

         procedure Add_Row
           (World : Harriet.Db.World.World_Type)
         is
            use Harriet.Constants, Harriet.Solar_System;
            --              use Harriet.Db;

            function T (S : String) return Values.Model_Value_Type
                        renames Values.Text_Value;
            function R (X : Real) return Values.Model_Value_Type
                        renames Values.Real_Value;
            function Q
              (X : Quantities.Quantity_Type)
               return Values.Model_Value_Type
            is (Values.Real_Value (Quantities.To_Real (X)));

            Colony : constant Harriet.Db.Colony.Colony_Type :=
              Harriet.Db.Colony.Get_By_World (World.Get_World_Reference);

         begin
            Model.Add_Row
              ((T (World.Name),
               R (World.Semimajor_Axis / Harriet.Solar_System.Earth_Orbit),
               R (World.Radius / Harriet.Solar_System.Earth_Radius),
               R (World.Surface_Gravity),
               R (World.Average_Temperature - Freezing_Point_Of_Water),
--                 R (World.Minimum_Temperature - Freezing_Point_Of_Water),
--                 R (World.Maximum_Temperature - Freezing_Point_Of_Water),
--                 R (World.Night_Temperature_Low - Freezing_Point_Of_Water),
--               R (World.Daytime_Temperature_High - Freezing_Point_Of_Water),
               (if World.Gas_Giant
                then R (0.0)
                else R (World.Surface_Pressure / Earth_Surface_Pressure)),
               T (Harriet.Db.World_Composition'Image (World.Composition)),
               T (Harriet.Db.World_Climate'Image (World.Climate)),
               Q ((if Colony.Has_Element
                 then Colony.Population else Quantities.Zero))));
         end Add_Row;

      begin
         for World of
           Harriet.Db.World.Select_By_Star_System (Star_System)
         loop
            Add_Row (World);
         end loop;
      end;

      Model.Last_Fetch := Harriet.Calendar.Clock;

   end Start;

end Harriet.UI.Models.Star_System;
