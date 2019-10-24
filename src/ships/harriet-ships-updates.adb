with Harriet.Calendar;
with Harriet.Logging;

with Harriet.Worlds;

with Harriet.Updates.Events;

with Harriet.Db.World;

package body Harriet.Ships.Updates is

   type Ship_Update is
     new Harriet.Updates.Update_Interface with
      record
         Ship : Harriet.Db.Ship_Reference;
      end record;

   overriding procedure Activate (Update : Ship_Update);

   procedure On_Arrival (Ship : Harriet.Db.Ship.Ship_Type);

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
               "moving to " & Harriet.Db.World.Get (Ship.Destination).Name);
            declare
               use Harriet.Calendar;
               Journey_Time : constant Duration :=
                 Harriet.Ships.Journey_Time
                   (Ship     => Get (Update.Ship),
                    Distance =>
                      Harriet.Worlds.Distance
                        (Ship.World, Ship.Destination));
               Arrival_Time : constant Time := Clock + Journey_Time;
            begin
               Harriet.Db.Ship.Update_Ship (Update.Ship)
                 .Set_Status (Moving)
                 .Set_Arrival (Arrival_Time)
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

   ----------------
   -- On_Arrival --
   ----------------

   procedure On_Arrival (Ship : Harriet.Db.Ship.Ship_Type) is
   begin
      Harriet.Logging.Log
        (Ship.Name,
         "arrived at " & Harriet.Worlds.Name (Ship.Destination));

      Harriet.Db.Ship.Update_Ship (Ship.Get_Ship_Reference)
        .Set_World (Ship.Destination)
        .Set_Destination (Harriet.Db.Null_World_Reference)
        .Set_Status (Harriet.Db.Idle)
        .Done;
   end On_Arrival;

   ------------
   -- Signal --
   ------------

   procedure Signal (Ship : Harriet.Db.Ship_Reference) is
   begin
      Harriet.Updates.Events.Update_With_Delay
        (0.0, Ship_Update'(Ship => Ship));
   end Signal;

end Harriet.Ships.Updates;
