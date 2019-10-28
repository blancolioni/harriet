with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Solar_System;
with Harriet.Logging;
with Harriet.Real_Images;

with Harriet.Ships;
with Harriet.Worlds;

with Harriet.Ships.Updates;

with Harriet.Db.Component;
with Harriet.Db.Faction;
with Harriet.Db.Goal;
with Harriet.Db.Module;
with Harriet.Db.Ship;
with Harriet.Db.World_Goal;

package body Harriet.Managers.Fleets is

   type Default_Fleet_Manager_Type is
     new Root_Manager_Type with
      record
         Faction : Harriet.Db.Faction_Reference;
      end record;

   overriding function Identifier
     (Manager : Default_Fleet_Manager_Type)
      return String
   is (Harriet.Db.Faction.Get (Manager.Faction).Name & " Fleet Manager");

   overriding procedure Activate
     (Manager : not null access Default_Fleet_Manager_Type);

   type World_Goal_Record is
      record
         Rec_Type : Harriet.Db.Record_Type;
         Goal     : Harriet.Db.World_Goal_Reference;
         World    : Harriet.Db.World_Reference;
         System   : Harriet.Db.Star_System_Reference;
         Assigned : Boolean;
      end record;

   package World_Goal_Lists is
     new Ada.Containers.Doubly_Linked_Lists (World_Goal_Record);

   type Ship_Record is
      record
         Ship     : Harriet.Db.Ship_Reference;
         World    : Harriet.Db.World_Reference;
         System   : Harriet.Db.Star_System_Reference;
         Assigned : Boolean;
      end record;

   package Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Ship_Record);

   procedure Assign_Ships_To_World_Goals
     (World_Goals     : in out World_Goal_Lists.List;
      Available_Ships : in out Ship_Lists.List;
      Score           : not null access
        function (World_Goal : World_Goal_Record;
                  Ship : Ship_Record)
      return Real);

   function Standard_Score
     (World_Goal : World_Goal_Record;
      Ship       : Ship_Record)
      return Real;

   procedure Assign_Ship_To_Goal
     (Ship : Harriet.Db.Ship_Reference;
      Goal : Harriet.Db.World_Goal_Reference);

   function Is_Fleet_Goal
     (Goal : Harriet.Db.Goal.Goal_Type)
      return Boolean;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Default_Fleet_Manager_Type)
   is
      World_Goals     : World_Goal_Lists.List;
      Available_Ships : Ship_Lists.List;
   begin
      Manager.Log
        (Manager.Identifier & " activating");

      for Ship of
        Harriet.Db.Ship.Select_By_Ship_Status
          (Manager.Faction, Harriet.Db.Idle)
      loop
         Manager.Log
           ("available ship: " & Ship.Name);
         Available_Ships.Append
           (Ship_Record'
              (Ship     => Ship.Get_Ship_Reference,
               World    => Ship.World,
               System   => Ship.Star_System,
               Assigned => False));
      end loop;

      for Goal of
        Harriet.Db.World_Goal.Select_Faction_Priority_Bounded_By_Priority
          (Status          => Harriet.Db.Waiting,
           Faction         => Manager.Faction,
           Start_Priority  => 1,
           Finish_Priority => 99)
      loop
         if Is_Fleet_Goal (Goal) then
            Manager.Log
              ("priority" & Natural'Image (Goal.Priority)
               & " goal: "
               & Harriet.Db.Record_Type'Image (Goal.Top_Record)
               & " "
               & Harriet.Worlds.Name (Goal.World));

            World_Goals.Append
              (World_Goal_Record'
                 (Rec_Type => Goal.Top_Record,
                  Goal     => Goal.Get_World_Goal_Reference,
                  World    => Goal.World,
                  System   =>
                    Harriet.Worlds.Star_System (Goal.World),
                  Assigned => False));
         end if;
      end loop;

      Assign_Ships_To_World_Goals
        (World_Goals     => World_Goals,
         Available_Ships => Available_Ships,
         Score           => Standard_Score'Access);

   end Activate;

   -------------------------
   -- Assign_Ship_To_Goal --
   -------------------------

   procedure Assign_Ship_To_Goal
     (Ship : Harriet.Db.Ship_Reference;
      Goal : Harriet.Db.World_Goal_Reference)
   is
      S : constant Harriet.Ships.Ship_Type'Class :=
        Harriet.Ships.Get (Ship);
      Goal_Rec : constant Harriet.Db.World_Goal.World_Goal_Type :=
        Harriet.Db.World_Goal.Get (Goal);
      Distance : constant Non_Negative_Real :=
        Harriet.Worlds.Distance (S.World, Goal_Rec.World)
        / Harriet.Solar_System.Earth_Orbit;
   begin
      Harriet.Logging.Log
        ("fleets",
         Harriet.Db.Ship.Get (Ship).Name
         & " assigned to "
         & Harriet.Db.Record_Type'Image (Goal_Rec.Top_Record)
         & " "
         & Harriet.Worlds.Name (Goal_Rec.World)
         & " distance "
         & Harriet.Real_Images.Approximate_Image
           (Distance)
         & " AU"
         & "; travel time "
         & Harriet.Real_Images.Approximate_Image
           (Distance / S.Maximum_System_Speed)
         & " days");

      Harriet.Db.World_Goal.Update_World_Goal (Goal)
        .Set_Status (Harriet.Db.In_Progress)
        .Done;

      Harriet.Db.Ship.Update_Ship (Ship)
        .Set_Destination (Goal_Rec.World)
        .Set_Status (Harriet.Db.Activating)
        .Set_Goal (Goal_Rec.Get_Goal_Reference)
        .Done;

      Harriet.Ships.Updates.Signal (Ship);

   end Assign_Ship_To_Goal;

   ---------------------------------
   -- Assign_Ships_To_World_Goals --
   ---------------------------------

   procedure Assign_Ships_To_World_Goals
     (World_Goals     : in out World_Goal_Lists.List;
      Available_Ships : in out Ship_Lists.List;
      Score           : not null access
        function (World_Goal : World_Goal_Record;
                  Ship : Ship_Record)
      return Real)
   is
      type Score_Record is
         record
            Score      : Real;
            World_Goal : World_Goal_Lists.Cursor;
            Ship       : Ship_Lists.Cursor;
         end record;

      package Score_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Score_Record);

      Scores : Score_Lists.List;

   begin
      for Goal in World_Goals.Iterate loop
         for Ship in Available_Ships.Iterate loop
            declare
               Goal_Rec : constant World_Goal_Record :=
                 World_Goal_Lists.Element (Goal);
               Ship_Rec : constant Ship_Record :=
                 Ship_Lists.Element (Ship);
               This_Score : constant Real :=
                 Score (Goal_Rec, Ship_Rec);
            begin
               Scores.Append
                 (Score_Record'
                    (Score      => This_Score,
                     World_Goal => Goal,
                     Ship       => Ship));
            end;
         end loop;
      end loop;

      declare
         function Better (Left, Right : Score_Record) return Boolean
         is (Left.Score > Right.Score);

         package Sorting is
           new Score_Lists.Generic_Sorting (Better);
      begin
         Sorting.Sort (Scores);
      end;

      for Item of Scores loop
         exit when Item.Score <= 0.0;
         declare
            Goal_Rec : World_Goal_Record renames
              World_Goals (Item.World_Goal);
            Ship_Rec : Ship_Record renames
              Available_Ships (Item.Ship);
         begin
            if not Goal_Rec.Assigned
              and then not Ship_Rec.Assigned
            then
               Goal_Rec.Assigned := True;
               Ship_Rec.Assigned := True;
               Assign_Ship_To_Goal (Ship_Rec.Ship, Goal_Rec.Goal);
            end if;
         end;
      end loop;

   end Assign_Ships_To_World_Goals;

   ---------------------------
   -- Default_Fleet_Manager --
   ---------------------------

   function Default_Fleet_Manager
     (Faction : Harriet.Db.Faction_Reference) return Manager_Type
   is
   begin
      return new Default_Fleet_Manager_Type'
        (Root_Manager_Type with
           Faction         => Faction);
   end Default_Fleet_Manager;

   -------------------
   -- Is_Fleet_Goal --
   -------------------

   function Is_Fleet_Goal
     (Goal : Harriet.Db.Goal.Goal_Type)
      return Boolean
   is
      use all type Harriet.Db.Record_Type;
   begin
      return Goal.Top_Record in
          R_Scan_World_Goal
        | R_Colonise_Goal
        | R_Explore_Goal;
   end Is_Fleet_Goal;

   --------------------
   -- Standard_Score --
   --------------------

   function Standard_Score
     (World_Goal : World_Goal_Record;
      Ship       : Ship_Record)
      return Real
   is
      use Harriet.Db;

      Result : Real := 0.0;

      Score : constant array (Record_Type, Record_Type) of Real :=
        (R_Scan_World_Goal =>
           (R_Scanner          => 100.0,
            R_Energy_Weapon    => -1.0,
            R_Missile_Bay      => -1.0,
            R_Missile_Launcher => -1.0,
            R_Engine           => 1.0,
            others             => 0.0),
         others            => (others => 0.0));

      Prerequisite : constant array (Record_Type, Record_Type) of Boolean :=
        (R_Scan_World_Goal =>
           (R_Scanner => True,
            R_Engine  => True,
            R_Tank    => True,
            others    => False),
         others            => (others => False));

      Have : array (Record_Type) of Boolean :=
        (others => False);

   begin
      for Module of
        Harriet.Db.Module.Select_By_Ship (Ship.Ship)
      loop
         declare
            Component : constant Harriet.Db.Component.Component_Type :=
              Harriet.Db.Component.Get (Module.Component);
            Top       : constant Harriet.Db.Record_Type :=
              Component.Top_Record;
         begin
            Result := Result + Score (World_Goal.Rec_Type, Top);
            Have (Top) := True;
         end;
      end loop;

      for R in Have'Range loop
         if Prerequisite (World_Goal.Rec_Type, R)
           and then not Have (R)
         then
            return 0.0;
         end if;
      end loop;

      if Ship.System = World_Goal.System then
         if Ship.World = World_Goal.World then
            Result := Result + 20.0;
         else
            Result := Result
              - Harriet.Worlds.Distance (Ship.World, World_Goal.World)
              / Harriet.Ships.Get (Ship.Ship).Maximum_System_Speed
              / Harriet.Solar_System.Earth_Orbit;
         end if;
      else
         Result := Result / 10.0;
      end if;

      return Result;

   end Standard_Score;

end Harriet.Managers.Fleets;
