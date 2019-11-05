with Harriet.Calendar;
with Harriet.Logging;
with Harriet.Real_Images;

with Harriet.Solar_System;

with Harriet.Factions;
with Harriet.Locations;
with Harriet.Star_Systems;
with Harriet.Worlds;

with Harriet.Managers;
with Harriet.Updates.Events;

with Harriet.Db.Goal;
with Harriet.Db.Scan_Star_Gate_Goal;
with Harriet.Db.Scan_World_Goal;
with Harriet.Db.Star_Gate;
with Harriet.Db.Star_System;
with Harriet.Db.World_Goal;

package body Harriet.Ships.Updates is

   type Ship_Update is
     new Harriet.Updates.Update_Interface with
      record
         Ship : Harriet.Db.Ship_Reference;
      end record;

   overriding procedure Activate (Update : Ship_Update);

   procedure On_Arrival (Ship : Harriet.Db.Ship.Ship_Type);

   procedure Scan_World
     (Ship  : Harriet.Db.Ship_Reference;
      World : Harriet.Db.World_Reference);

   procedure Enter_Star_Gate
     (Ship  : Harriet.Db.Ship_Reference;
      Gate  : Harriet.Db.Star_Gate_Reference);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Update : Ship_Update) is
      use all type Harriet.Db.Ship_Status;
      use type Harriet.Calendar.Time;

      Ship : constant Harriet.Db.Ship.Ship_Type :=
        Harriet.Db.Ship.Get (Update.Ship);
   begin
      Harriet.Logging.Log
        ("update", Ship.Name);
      case Harriet.Db.Ship.Get (Update.Ship).Status is
         when Idle =>
            Harriet.Logging.Log (Ship.Name, "idling");
         when Activating =>
            Harriet.Logging.Log
              (Ship.Name,
               "moving to " & Harriet.Locations.Show (Ship.Destination));
            declare
               use Harriet.Calendar;
               Distance : constant Non_Negative_Real :=
                 Harriet.Locations.Distance
                   (Ship.Location, Ship.Destination);
               Journey_Time : constant Duration :=
                 Harriet.Ships.Journey_Time
                   (Ship     => Get (Update.Ship),
                    Distance => Distance);
               Arrival_Time : constant Time := Clock + Journey_Time;
            begin
               Harriet.Logging.Log
                 (Ship.Name,
                  "distance "
                  & Harriet.Real_Images.Approximate_Image
                    (Distance / Harriet.Solar_System.Earth_Orbit)
                  & " AU; journey time "
                  & Harriet.Real_Images.Approximate_Image
                    (Real (Journey_Time) / 3600.0)
                  & " hours; arrival "
                  & Harriet.Calendar.Image (Arrival_Time, True));
               Harriet.Db.Ship.Update_Ship (Update.Ship)
                 .Set_Status (Moving)
                 .Set_Arrival (Arrival_Time)
                 .Set_World (Harriet.Db.Null_World_Reference)
                 .Done;
               Harriet.Updates.Events.Update_At
                 (Arrival_Time, Update);
            end;

         when Moving =>
            if Harriet.Calendar.Clock >= Ship.Arrival then
               On_Arrival (Ship);
            else
               Harriet.Updates.Events.Update_At
                 (Ship.Arrival, Update);
            end if;

         when Surveying =>

            declare
               Scan_Start : constant Harriet.Calendar.Time := Ship.Start;
               Scan_Total : constant Non_Negative_Real :=
                 Get (Ship).Current_Scan_Capability
                 * Harriet.Calendar.To_Days
                 (Harriet.Calendar.Clock - Scan_Start);

               Minimum    : constant Non_Negative_Real :=
                 100.0 / Scan_Total;
               Target     : constant Unit_Real :=
                 Harriet.Db.Scan_World_Goal.Get_Scan_World_Goal
                   (Ship.Goal)
                 .Minimum_Deposit;
            begin
               Harriet.Logging.Log
                 (Ship.Name,
                  "scan started " & Harriet.Calendar.Image (Scan_Start, True)
                  & "; total scan "
                  & Harriet.Real_Images.Approximate_Image (Scan_Total)
                  & "; minimum detected deposit "
                  & Harriet.Real_Images.Approximate_Image (Minimum));

               if Minimum <= 1.0 then
                  Harriet.Factions.Discover_World_Deposits
                    (Faction => Ship.Faction,
                     World   => Ship.World,
                     Minimum => Minimum);
               end if;

               if Minimum <= Target then
                  Harriet.Db.Ship.Update_Ship (Ship.Get_Ship_Reference)
                    .Set_Goal (Harriet.Db.Null_Goal_Reference)
                    .Set_Status (Harriet.Db.Idle)
                    .Done;
                  Harriet.Managers.Signal
                    (Faction => Get (Ship).Owner,
                     Area    => Harriet.Managers.Fleet);
               else
                  Harriet.Updates.Events.Update_With_Delay
                    (Wait   => Harriet.Calendar.Days (1),
                     Update => Update);
               end if;
            end;

         when Training =>
            Harriet.Logging.Log (Ship.Name, "training");
            Harriet.Db.Ship.Update_Ship (Update.Ship)
              .Set_Training
                (Ship.Training + (1.0 - Ship.Training) * 0.01)
              .Done;

            Harriet.Updates.Events.Update_With_Delay (1.0, Update);

         when Repairing =>
            Harriet.Logging.Log (Ship.Name, "repairing");
            Harriet.Updates.Events.Update_With_Delay
              (Harriet.Calendar.Days (1), Update);

         when Destroyed =>

            Harriet.Logging.Log (Ship.Name, "destroyed");

      end case;
   end Activate;

   ---------------------
   -- Enter_Star_Gate --
   ---------------------

   procedure Enter_Star_Gate
     (Ship  : Harriet.Db.Ship_Reference;
      Gate  : Harriet.Db.Star_Gate_Reference)
   is
      use type Harriet.Calendar.Time;
      Origin : constant Harriet.Db.Star_System_Reference :=
        Harriet.Db.Star_Gate.Get (Gate).From;
      Destination : constant Harriet.Db.Star_System_Reference :=
        Harriet.Db.Star_Gate.Get (Gate).To;
      Distance    : constant Non_Negative_Real :=
        Harriet.Star_Systems.Distance (Origin, Destination);
      Arrival : constant Harriet.Calendar.Time :=
        Harriet.Calendar.Clock
          + Harriet.Calendar.Days (Distance);
   begin
      Harriet.Logging.Log
        (Harriet.Db.Ship.Get (Ship).Name,
         "travelling via gate" & Harriet.Db.To_String (Gate)
         & " to "
         & Harriet.Db.Star_System.Get
           (Harriet.Db.Star_Gate.Get (Gate).To)
         .Name);
      Harriet.Db.Ship.Update_Ship (Ship)
        .Set_Arrival (Arrival)
        .Set_Status (Harriet.Db.Moving)
        .Set_Star_System (Harriet.Db.Null_Star_System_Reference)
        .Done;

      Harriet.Locations.Set_System_Location
        (Harriet.Db.Ship.Get (Ship).Destination,
         Destination, 0.0, 0.0, 0.0);

      Harriet.Updates.Events.Update_At
        (Arrival, Ship_Update'(Ship => Ship));

   end Enter_Star_Gate;

   ----------------
   -- On_Arrival --
   ----------------

   procedure On_Arrival (Ship : Harriet.Db.Ship.Ship_Type) is
      World : constant Harriet.Db.World_Reference :=
        (if Harriet.Locations.Has_World (Ship.Destination)
         then Harriet.Locations.Get_World (Ship.Destination)
         else Harriet.Db.Null_World_Reference);
   begin
      Harriet.Logging.Log
        (Ship.Name,
         "arrived at " & Harriet.Locations.Show (Ship.Destination));

      if Harriet.Locations.Has_World (Ship.Destination) then
         Harriet.Worlds.Check_Surface (World);
      end if;

      Harriet.Db.Ship.Update_Ship (Ship.Get_Ship_Reference)
        .Set_World (World)
        .Set_Primary_Massive
          (Harriet.Locations.Get_Primary_Massive
             (Ship.Destination))
        .Set_Status (Harriet.Db.Idle)
        .Done;

      Harriet.Locations.Clear (Ship.Destination);

      declare
         use Harriet.Db;
      begin
         if Ship.Goal /= Null_Goal_Reference then
            case Harriet.Db.Goal.Get (Ship.Goal).Top_Record is
               when R_Scan_World_Goal =>
                  Scan_World (Ship.Get_Ship_Reference,
                              Harriet.Db.World_Goal.Get_World_Goal
                                (Ship.Goal).World);
               when R_Scan_Star_Gate_Goal =>
                  Enter_Star_Gate
                    (Ship.Get_Ship_Reference,
                     Harriet.Db.Scan_Star_Gate_Goal.Get_Scan_Star_Gate_Goal
                       (Ship.Goal)
                     .Star_Gate);

               when others =>
                  Harriet.Db.Ship.Update_Ship (Ship.Get_Ship_Reference)
                    .Set_Goal (Harriet.Db.Null_Goal_Reference)
                    .Done;
            end case;
         end if;
      end;

      Harriet.Managers.Signal
        (Faction => Get (Ship).Owner,
         Area    => Harriet.Managers.Fleet);

   end On_Arrival;

   ----------------
   -- Scan_World --
   ----------------

   procedure Scan_World
     (Ship  : Harriet.Db.Ship_Reference;
      World : Harriet.Db.World_Reference)
   is
      Capability : constant Non_Negative_Real :=
        Get (Ship).Current_Scan_Capability;
   begin
      if Capability > 0.0 then
         Harriet.Logging.Log
           (Get (Ship).Name,
            "scanning " & Harriet.Worlds.Name (World)
            & " with capability "
            & Harriet.Real_Images.Approximate_Image (Capability));
         Harriet.Db.Ship.Update_Ship (Ship)
           .Set_Status (Harriet.Db.Surveying)
           .Set_Start  (Harriet.Calendar.Clock)
           .Done;

         Harriet.Updates.Events.Update_With_Delay
           (Wait   => Harriet.Calendar.Days (1),
            Update => Ship_Update'(Ship => Ship));

      end if;
   end Scan_World;

   ------------
   -- Signal --
   ------------

   procedure Signal (Ship : Harriet.Db.Ship_Reference) is
   begin
      Harriet.Updates.Events.Update_With_Delay
        (0.0, Ship_Update'(Ship => Ship));
   end Signal;

end Harriet.Ships.Updates;
