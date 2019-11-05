with Ada.Text_IO;

with Harriet.Calendar;
with Harriet.Orbits;
with Harriet.Random;
with Harriet.Solar_System;

with Harriet.Locations;
with Harriet.Worlds;

with Harriet.Db.Component;
with Harriet.Db.Design_Module;
with Harriet.Db.Module;
with Harriet.Db.World;

with Harriet.Db.Crew_Quarters;
with Harriet.Db.Energy_Weapon;
with Harriet.Db.Engine;
with Harriet.Db.Generator;
with Harriet.Db.Hold;
with Harriet.Db.Jump_Drive;
with Harriet.Db.Missile_Launcher;
with Harriet.Db.Scanner;
with Harriet.Db.Shield_Generator;
with Harriet.Db.Tank;

with Harriet.Db.Crew_Module;
with Harriet.Db.Engine_Module;
with Harriet.Db.Energy_Weapon_Module;
with Harriet.Db.Generator_Module;
with Harriet.Db.Hold_Module;
with Harriet.Db.Jump_Module;
with Harriet.Db.Launcher_Module;
with Harriet.Db.Scanner_Module;
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
         when R_Crew_Quarters =>
            Harriet.Db.Crew_Module.Create
              (Ship          => Ship,
               Component     => Component,
               Crew          => Base.Crew,
               Condition     => 1.0,
               Crew_Quarters =>
                  Harriet.Db.Crew_Quarters.Get_Crew_Quarters (Component)
               .Get_Crew_Quarters_Reference);

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

         when R_Scanner =>
            Harriet.Db.Scanner_Module.Create
              (Ship      => Ship,
               Component => Component,
               Crew      => Base.Crew,
               Condition => 1.0,
               Scanner =>
                  Harriet.Db.Scanner.Get_Scanner (Component)
               .Get_Scanner_Reference);

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
      use type Harriet.Calendar.Time;
      World_Rec : constant Harriet.Db.World.World_Type :=
        Harriet.Db.World.Get (World);
      Orbit     : constant Long_Float :=
        World_Rec.Radius
          + 1000.0 * (3000.0 + 1000.0 * Harriet.Random.Unit_Random);
      Period    : constant Non_Negative_Real :=
        Harriet.Orbits.Period (World_Rec.Mass, Orbit);
      Location : constant Harriet.Db.Location_Reference :=
        Harriet.Locations.New_Location;
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
           Location          => Location,
           Mass              => Harriet.Ships.Design_Mass (Design),
           Star_System       => World_Rec.Star_System,
           World             => World,
           Home              => World,
           Primary_Massive   => World_Rec.Get_Massive_Object_Reference,
           Semimajor_Axis    => Orbit,
           Epoch             =>
             Harriet.Calendar.Clock
           - Duration (Harriet.Random.Unit_Random * Period),
           Period            => Period,
           Eccentricity      => 0.0,
           Status            => Harriet.Db.Idle,
           Training          => 1.0,
           Fuel              => Design_Fuel_Mass (Design),
           Departure         => Harriet.Calendar.Clock,
           Arrival           => Harriet.Calendar.Clock,
           Start             => Harriet.Calendar.Clock,
           Destination       => Harriet.Locations.New_Location,
           Goal              => Harriet.Db.Null_Goal_Reference);
   begin
      Harriet.Locations.Set_World_Orbit_Location (Location, World, Orbit);
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

   -----------------------------
   -- Current_Scan_Capability --
   -----------------------------

   function Current_Scan_Capability
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
      Scan_Capability : Non_Negative_Real := 0.0;
      Individual      : array (1 .. 20) of Non_Negative_Real :=
        (others => 0.0);
      Count           : Natural := 0;
   begin
      for Module of
        Harriet.Db.Scanner_Module.Select_By_Ship (Ship.Reference)
      loop
         declare
            Scanner    : constant Harriet.Db.Scanner.Scanner_Type :=
              Harriet.Db.Scanner.Get (Module.Scanner);
            Base_Scan  : constant Non_Negative_Real :=
              Non_Negative_Real (Scanner.Scan);
            Condition  : constant Unit_Real :=
              Module.Condition;
            Final_Scan : constant Non_Negative_Real :=
              Base_Scan * Condition ** 2;
            Index      : Natural := Count;
         begin
            Count := Count + 1;
            while Index > 0
              and then Final_Scan > Individual (Index)
            loop
               Individual (Index + 1) := Individual (Index);
               Index := Index - 1;
            end loop;
            Individual (Index + 1) := Final_Scan;
         end;
         exit when Count = Individual'Last;
      end loop;

      declare
         Factor : Unit_Real := 1.0;
      begin
         for Scan of Individual (1 .. Count) loop
            Scan_Capability := Scan_Capability + Scan * Factor;
            Factor := Factor / 2.0;
         end loop;
      end;
      return Scan_Capability;
   end Current_Scan_Capability;

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

   -----------------
   -- Design_Crew --
   -----------------

   function Design_Crew
     (Design : Harriet.Db.Ship_Design_Reference)
      return Natural
   is
   begin
      return Crew : Natural := 0 do
         for Module of
           Harriet.Db.Design_Module.Select_By_Ship_Design (Design)
         loop
            Crew := Crew +
              Harriet.Db.Component.Get (Module.Component).Crew;
         end loop;
      end return;
   end Design_Crew;

   function Design_Crew_Berths
     (Design : Harriet.Db.Ship_Design_Reference)
      return Natural
   is
      use all type Harriet.Db.Record_Type;
   begin
      return Crew : Natural := 0 do
         for Module of
           Harriet.Db.Design_Module.Select_By_Ship_Design (Design)
         loop
            if Harriet.Db.Component.Get (Module.Component).Top_Record
              = R_Crew_Quarters
            then
               Crew := Crew +
                 Natural
                   (Harriet.Db.Crew_Quarters.Get_Crew_Quarters
                      (Module.Component)
                    .Capacity);
            end if;
         end loop;
      end return;
   end Design_Crew_Berths;

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

   ---------------------------------
   -- Design_Maximum_System_Speed --
   ---------------------------------

   function Design_Maximum_System_Speed
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real
   is
   begin
      return Design_Impulse (Design)
        / (Design_Mass (Design) + Design_Fuel_Mass (Design)
           + Design_Cargo_Volume (Design));
   end Design_Maximum_System_Speed;

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

   ------------------
   -- Journey_Time --
   ------------------

   function Journey_Time
     (Ship     : Ship_Type'Class;
      Distance : Non_Negative_Real)
      return Duration
   is
   begin
      return Harriet.Calendar.Days
          (Distance
           / Harriet.Solar_System.Earth_Orbit
           / Ship.Maximum_System_Speed);
   end Journey_Time;

   --------------------------
   -- Maximum_System_Speed --
   --------------------------

   function Maximum_System_Speed
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Ship.Total_Impulse / Ship.Current_Mass;
   end Maximum_System_Speed;

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
