with Ada.Text_IO;

with Harriet.Calendar;
with Harriet.Random;

with Harriet.Worlds;

with Harriet.Db.Component;
with Harriet.Db.Design_Module;
with Harriet.Db.Module;
with Harriet.Db.World;

with Harriet.Db.Energy_Weapon;
with Harriet.Db.Engine;
with Harriet.Db.Generator;
with Harriet.Db.Hold;
with Harriet.Db.Jump_Drive;
with Harriet.Db.Missile_Launcher;
with Harriet.Db.Shield_Generator;
with Harriet.Db.Tank;

with Harriet.Db.Engine_Module;
with Harriet.Db.Energy_Weapon_Module;
with Harriet.Db.Generator_Module;
with Harriet.Db.Hold_Module;
with Harriet.Db.Jump_Module;
with Harriet.Db.Launcher_Module;
with Harriet.Db.Shield_Module;
with Harriet.Db.Tank_Module;

package body Harriet.Ships is

   procedure Create_Module
     (Ship      : Harriet.Db.Ship_Reference;
      Component : Harriet.Db.Component_Reference);

   -------------------
   -- Create_Module --
   -------------------

   procedure Create_Module
     (Ship      : Harriet.Db.Ship_Reference;
      Component : Harriet.Db.Component_Reference)
   is
      use all type Harriet.Db.Record_Type;
      Base : constant Harriet.Db.Component.Component_Type :=
        Harriet.Db.Component.Get (Component);
      Top : constant Harriet.Db.Record_Type := Base.Top_Record;
   begin
      case Top is
         when R_Engine =>
            Harriet.Db.Engine_Module.Create
              (Ship      => Ship,
               Component => Component,
               Crew      => Base.Crew,
               Condition => 1.0,
               Engine    =>
                  Harriet.Db.Engine.Get_Engine (Component)
               .Get_Engine_Reference);

         when R_Hold =>
            Harriet.Db.Hold_Module.Create
              (Ship      => Ship,
               Component => Component,
               Crew      => Base.Crew,
               Condition => 1.0,
               Hold      =>
                  Harriet.Db.Hold.Get_Hold (Component).Get_Hold_Reference);

         when R_Tank =>
            Harriet.Db.Tank_Module.Create
              (Ship      => Ship,
               Component => Component,
               Crew      => Base.Crew,
               Condition => 1.0,
               Tank      =>
                  Harriet.Db.Tank.Get_Tank (Component).Get_Tank_Reference);

         when R_Missile_Launcher =>
            Harriet.Db.Launcher_Module.Create
              (Ship      => Ship,
               Component => Component,
               Crew      => Base.Crew,
               Condition => 1.0,
               Missile_Launcher =>
                  Harriet.Db.Missile_Launcher.Get_Missile_Launcher (Component)
               .Get_Missile_Launcher_Reference);

         when R_Energy_Weapon =>
            Harriet.Db.Energy_Weapon_Module.Create
              (Ship      => Ship,
               Component => Component,
               Crew      => Base.Crew,
               Condition => 1.0,
               Energy_Weapon =>
                  Harriet.Db.Energy_Weapon.Get_Energy_Weapon (Component)
               .Get_Energy_Weapon_Reference);

         when R_Generator =>
            Harriet.Db.Generator_Module.Create
              (Ship      => Ship,
               Component => Component,
               Crew      => Base.Crew,
               Condition => 1.0,
               Generator =>
                  Harriet.Db.Generator.Get_Generator (Component)
               .Get_Generator_Reference);

         when R_Shield_Generator =>
            Harriet.Db.Shield_Module.Create
              (Ship      => Ship,
               Component => Component,
               Crew      => Base.Crew,
               Condition => 1.0,
               Shield_Generator =>
                  Harriet.Db.Shield_Generator.Get_Shield_Generator
                 (Component)
               .Get_Shield_Generator_Reference);

         when R_Jump_Drive =>
            Harriet.Db.Jump_Module.Create
              (Ship             => Ship,
               Component        => Component,
               Crew             => Base.Crew,
               Condition        => 1.0,
               Jump_Drive =>
                  Harriet.Db.Jump_Drive.Get_Jump_Drive (Component)
               .Get_Jump_Drive_Reference);

         when others =>
            raise Constraint_Error with
              "not a supported module type: "
              & Top'Image;

      end case;
   end Create_Module;

   -----------------
   -- Create_Ship --
   -----------------

   procedure Create_Ship
     (Owner   : Harriet.Db.Faction_Reference;
      World   : Harriet.Db.World_Reference;
      Design  : Harriet.Db.Ship_Design_Reference;
      Manager : String;
      Name    : String)
   is
      World_Rec : constant Harriet.Db.World.World_Type :=
        Harriet.Db.World.Get (World);
      Orbit     : constant Long_Float :=
        World_Rec.Radius
          + 1000.0 * (3000.0 + 1000.0 * Harriet.Random.Unit_Random);
      Ship         : constant Harriet.Db.Ship_Reference :=
        Harriet.Db.Ship.Create
          (Name              => Name,
           Active            => True,
           Scheduled         => False,
           Next_Event        => Harriet.Calendar.Clock,
           Manager           => Manager,
           Updates_Started   => True,
           Next_Update       => Harriet.Calendar.Clock,
           Faction           => Owner,
           Star_System       => World_Rec.Star_System,
           World             => World,
           Home              => World,
           Orbit             => Orbit,
           Alive             => True,
           Training          => 1.0,
           Fuel              => Design_Fuel_Mass (Design),
           Destination       => Harriet.Db.Null_World_Reference);
   begin
      for Design_Module of
        Harriet.Db.Design_Module.Select_By_Ship_Design
          (Design)
      loop
         Create_Module (Ship, Design_Module.Component);
      end loop;

      if True then
         declare
            S : constant Ship_Type := Get (Ship);
         begin
            Ada.Text_IO.Put_Line
              (S.Name & " in orbit above "
               & Harriet.Worlds.Name (S.World));
         end;
      end if;
   end Create_Ship;

   ------------------
   -- Current_Mass --
   ------------------

   function Current_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Ship.Dry_Mass;
   end Current_Mass;

   -------------------------
   -- Design_Cargo_Volume --
   -------------------------

   function Design_Cargo_Volume
     (Design : Harriet.Db.Ship_Design_Reference) return Non_Negative_Real
   is
      use all type Harriet.Db.Record_Type;
   begin
      return Volume : Non_Negative_Real := 0.0 do
         for Module of
           Harriet.Db.Design_Module.Select_By_Ship_Design (Design)
         loop
            if Harriet.Db.Component.Get (Module.Component).Top_Record
              = R_Hold
            then
               Volume := Volume
                 + Harriet.Db.Hold.Get_Hold (Module.Component).Capacity;
            end if;
         end loop;
      end return;
   end Design_Cargo_Volume;

   ----------------------
   -- Design_Fuel_Mass --
   ----------------------

   function Design_Fuel_Mass
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real
   is
      use all type Harriet.Db.Record_Type;
   begin
      return Mass : Non_Negative_Real := 0.0 do
         for Module of
           Harriet.Db.Design_Module.Select_By_Ship_Design (Design)
         loop
            if Harriet.Db.Component.Get (Module.Component).Top_Record
              = R_Tank
            then
               Mass := Mass
                 + Harriet.Db.Tank.Get_Tank (Module.Component).Capacity;
            end if;
         end loop;
      end return;
   end Design_Fuel_Mass;

   --------------------
   -- Design_Impulse --
   --------------------

   function Design_Impulse
     (Design : Harriet.Db.Ship_Design_Reference) return Non_Negative_Real
   is
      use all type Harriet.Db.Record_Type;
   begin
      return Impulse : Non_Negative_Real := 0.0 do
         for Module of
           Harriet.Db.Design_Module.Select_By_Ship_Design (Design)
         loop
            if Harriet.Db.Component.Get (Module.Component).Top_Record
              = R_Engine
            then
               Impulse := Impulse
                 + Harriet.Db.Engine.Get_Engine (Module.Component).Impulse;
            end if;
         end loop;
      end return;
   end Design_Impulse;

   -----------------
   -- Design_Mass --
   -----------------

   function Design_Mass
     (Design : Harriet.Db.Ship_Design_Reference) return Non_Negative_Real
   is
   begin
      return Mass : Non_Negative_Real := 0.0 do
         for Module of
           Harriet.Db.Design_Module.Select_By_Ship_Design (Design)
         loop
            Mass := Mass +
              Harriet.Db.Component.Get (Module.Component).Mass;
         end loop;
      end return;
   end Design_Mass;

   --------------
   -- Dry_Mass --
   --------------

   function Dry_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Mass : Non_Negative_Real := 0.0 do
         for Module of
           Harriet.Db.Module.Select_By_Ship (Ship.Reference)
         loop
            Mass := Mass +
              Harriet.Db.Component.Get (Module.Component).Mass;
         end loop;
      end return;
   end Dry_Mass;

   ---------------
   -- Tank_Size --
   ---------------

   function Tank_Size
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Size : Non_Negative_Real := 0.0 do
         for Module of
           Harriet.Db.Tank_Module.Select_By_Ship (Ship.Reference)
         loop
            Size := Size
              + Harriet.Db.Tank.Get (Module.Tank).Capacity;
         end loop;
      end return;
   end Tank_Size;

   -------------------
   -- Total_Impulse --
   -------------------

   function Total_Impulse
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
      use all type Harriet.Db.Record_Type;
   begin
      return Impulse : Non_Negative_Real := 0.0 do
         for Module of
           Harriet.Db.Module.Select_By_Ship (Ship.Reference)
         loop
            if Harriet.Db.Component.Get (Module.Component).Top_Record
              = R_Engine
            then
               Impulse := Impulse
                 + Harriet.Db.Engine.Get_Engine (Module.Component).Impulse
                 * Module.Condition ** 2;
            end if;
         end loop;
      end return;
   end Total_Impulse;

end Harriet.Ships;
