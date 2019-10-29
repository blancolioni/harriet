with Ada.Containers.Vectors;
with Ada.Strings.Fixed;

with Harriet.UI.Models.Data_Source;
with Harriet.UI.Models.Values;

with Harriet.Color;
with Harriet.Solar_System;

with Harriet.Factions;
with Harriet.Star_Systems;

with Harriet.Db.Star;
with Harriet.Db.Star_Gate;
with Harriet.Db.Star_System;

package body Harriet.UI.Models.Galaxy is

   Max_Gates : constant := 12;
   subtype Gate_Index is Integer range 1 .. Max_Gates;

   type Galaxy_Model_Type is
     new Harriet.UI.Models.Data_Source.Simple_Data_Source_Model with
      record
         null;
      end record;

   overriding procedure Start
     (Model     : in out Galaxy_Model_Type;
      User      : Harriet.Db.User_Reference;
      Arguments : String);

   overriding function Name
     (Model : Galaxy_Model_Type)
      return String
   is ("galaxy-data-source");

   overriding function Default_View_Name
     (Model : Galaxy_Model_Type)
      return String
   is ("Galaxy");

   overriding function Changed
     (Model : Galaxy_Model_Type)
      return Boolean
   is (False);

   ------------------------
   -- Galaxy_Model --
   ------------------------

   function Galaxy_Model
      return Root_Harriet_Model'Class
   is
   begin
      return Model : Galaxy_Model_Type;
   end Galaxy_Model;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Model     : in out Galaxy_Model_Type;
      User      : Harriet.Db.User_Reference;
      Arguments : String)
   is
      pragma Unreferenced (Arguments);
      use Harriet.Db;

      Faction : constant Harriet.Factions.Faction_Type'Class :=
        Harriet.Factions.Get_User_Faction (User);
      Capital : constant Harriet.Db.Star_System_Reference :=
        (if Faction.Has_Element
         then Faction.Capital_System
         else Harriet.Db.Null_Star_System_Reference);
      Position : constant Harriet.Star_Systems.Interstellar_Position :=
        (if Capital /= Harriet.Db.Null_Star_System_Reference
         then Harriet.Star_Systems.Position (Capital)
         else (0.0, 0.0, 0.0));

      type System_Record is
         record
            Reference : Harriet.Db.Star_System_Reference;
         end record;

      package System_Vectors is
        new Ada.Containers.Vectors (Positive, System_Record);

      Systems  : System_Vectors.Vector;

   begin
      Model.Add_Column
        (Id       => "name",
         Label    => "Name",
         Col_Type => Values.Text_Type);
      Model.Add_Column
        (Id       => "x",
         Label    => "X",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "y",
         Label    => "Y",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "z",
         Label    => "Z",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "mass",
         Label    => "Mass",
         Col_Type => Values.Real_Type);

      Model.Add_Column
        (Id       => "color",
         Label    => "Color",
         Col_Type => Values.Text_Type);

      for I in Gate_Index loop
         Model.Add_Column
           (Id       =>
              "gate" & Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Left),
            Label    => "Gate" & I'Image,
            Col_Type => Values.Text_Type);
      end loop;

      if Faction.Has_Element then
         Systems.Append ((Reference => Capital));
         for Star_Gate of
           Harriet.Db.Star_Gate.Select_By_From (Capital)
         loop
            Systems.Append ((Reference => Star_Gate.To));
         end loop;
      else
         for Star_System of Harriet.Db.Star_System.Scan_By_Name loop
            Systems.Append
              ((Reference => Star_System.Get_Star_System_Reference));
         end loop;
      end if;

      declare

         procedure Add_Row
           (Star_System : Harriet.Db.Star_System.Star_System_Type);

         -------------
         -- Add_Row --
         -------------

         procedure Add_Row
           (Star_System : Harriet.Db.Star_System.Star_System_Type)
         is
            function T (S : String) return Values.Model_Value_Type
                        renames Values.Text_Value;
            function R (X : Real) return Values.Model_Value_Type
                        renames Values.Real_Value;

            Star : constant Harriet.Db.Star.Star_Type :=
              Harriet.Db.Star.First_By_Star_System
                (Star_System.Get_Star_System_Reference);
            Color : constant String :=
              Harriet.Color.To_Html_String
                (Star.Red, Star.Green, Star.Blue);
            Gates : Data_Source.Model_Value_Array (Gate_Index) :=
              (others => Values.Null_Value);
            Next_Gate : Natural := 0;
         begin
            for Star_Gate of
              Harriet.Db.Star_Gate.Select_By_From
                (Star_System.Get_Star_System_Reference)
            loop
               for I in 1 .. Systems.Last_Index loop
                  if Systems.Element (I).Reference = Star_Gate.To then
                     Next_Gate := Next_Gate + 1;
                     Gates (Next_Gate) := Values.Integer_Value (I);
                     exit;
                  end if;
               end loop;
            end loop;

            declare
               use type Data_Source.Model_Value_Array;
            begin
               Model.Add_Row
                 ((T (Star_System.Name),
                  R (Star_System.X - Position.X),
                  R (Star_System.Y - Position.Y),
                  R (Star_System.Z - Position.Z),
                  R (Star.Mass / Harriet.Solar_System.Solar_Mass),
                  T (Color))
                  & Gates);
            end;
         end Add_Row;

      begin
         for I in 1 .. Systems.Last_Index loop
            declare
               System : constant Harriet.Db.Star_System.Star_System_Type :=
                 Harriet.Db.Star_System.Get (Systems.Element (I).Reference);
            begin
               Add_Row (System);
            end;
         end loop;
      end;

   end Start;

end Harriet.UI.Models.Galaxy;
