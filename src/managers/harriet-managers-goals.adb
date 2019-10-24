with Harriet.Logging;

with Harriet.Worlds;

with Harriet.Db.Available_Commodity;
with Harriet.Db.Colonise_Goal;
with Harriet.Db.Colony;
with Harriet.Db.Deposit;
with Harriet.Db.Faction;
with Harriet.Db.Resource;
with Harriet.Db.Resource_Goal;
with Harriet.Db.Scan_System_Goal;
with Harriet.Db.Scan_World_Goal;
with Harriet.Db.Transport_Goal;
with Harriet.Db.World_Knowledge;

package body Harriet.Managers.Goals is

   --------------------------
   -- Add_System_Scan_Goal --
   --------------------------

   procedure Add_System_Scan_Goal
     (Faction  : Harriet.Db.Faction_Reference;
      Priority : Priority_Type;
      System   : Harriet.Db.Star_System_Reference)
   is
   begin
      Harriet.Db.Scan_System_Goal.Create
        (Status      => Harriet.Db.Waiting,
         Faction     => Faction,
         Priority    => Positive (Priority),
         Star_System => System);
      Harriet.Managers.Signal
        (Faction => Faction,
         Area    => Harriet.Managers.Fleet);

   end Add_System_Scan_Goal;

   -------------------------
   -- Add_World_Scan_Goal --
   -------------------------

   procedure Add_World_Scan_Goal
     (Faction  : Harriet.Db.Faction_Reference;
      Priority : Priority_Type;
      World    : Harriet.Db.World_Reference)
   is
   begin
      if not Harriet.Db.Scan_World_Goal.Is_Scan_World_Goal
        (Faction, World)
      then
         Harriet.Db.Scan_World_Goal.Create
           (Status      => Harriet.Db.Waiting,
            Faction     => Faction,
            Priority    => Positive (Priority),
            World       => World);
         Harriet.Managers.Signal
           (Faction => Faction,
            Area    => Harriet.Managers.Fleet);
      end if;
   end Add_World_Scan_Goal;

   ---------------------------
   -- Colony_Needs_Resource --
   ---------------------------

   procedure Colony_Needs_Resource
     (Faction  : Harriet.Db.Faction_Reference;
      Priority : Priority_Type;
      Colony   : Harriet.Db.Colony_Reference;
      Resource : Harriet.Db.Resource_Reference;
      Quantity : Harriet.Quantities.Quantity_Type)
   is
      use Harriet.Quantities;
      Commodity : constant Harriet.Db.Commodity_Reference :=
        Harriet.Db.Resource.Get (Resource).Get_Commodity_Reference;
      World : constant Harriet.Db.World_Reference :=
        Harriet.Db.Colony.Get (Colony).World;

      Remaining : Quantity_Type := Quantity;
   begin
      Harriet.Logging.Log
        ("goals",
         Harriet.Db.Faction.Get (Faction).Name
         & " colony on "
         & Harriet.Worlds.Name (World)
         & " requires "
         & Harriet.Quantities.Show (Quantity)
         & " "
         & Harriet.Db.Resource.Get (Resource).Tag);

      for Receive of
        Harriet.Db.Transport_Goal.Select_By_Receive_Commodity
          (World, Commodity)
      loop
         Harriet.Logging.Log
           ("goals",
            "already receiving "
            & Harriet.Quantities.Show (Receive.Quantity)
            & " "
            & Harriet.Db.Resource.Get (Resource).Tag
            & " from "
            & Harriet.Worlds.Name (Receive.From));
         Remaining := Remaining - Min (Remaining, Receive.Quantity);
      end loop;

      if Remaining = Zero then
         return;
      end if;

      for Check_Colony of
        Harriet.Db.Colony.Select_By_Faction (Faction)
      loop
         declare
            use Harriet.Db.Available_Commodity;
            Available : constant Available_Commodity_Type :=
              Harriet.Db.Available_Commodity.Get_By_Available_Commodity
                (Check_Colony.Get_Colony_Reference, Commodity);
         begin
            if Available.Has_Element then
               declare
                  Supply : constant Quantity_Type :=
                    Min (Remaining, Available.Quantity);
               begin
                  Harriet.Logging.Log
                    ("goals",
                     Harriet.Db.Faction.Get (Faction).Name
                     & " colony on "
                     & Harriet.Worlds.Name (World)
                     & " transporting "
                     & Show (Supply)
                     & " "
                     & Harriet.Db.Resource.Get (Resource).Tag
                     & " from "
                     & Harriet.Worlds.Name (Check_Colony.World));

                  Remaining := Remaining - Supply;

                  Harriet.Db.Transport_Goal.Create
                    (Status    => Harriet.Db.Waiting,
                     Faction   => Faction,
                     Priority  => Positive (Priority),
                     Commodity => Commodity,
                     Quantity  => Supply,
                     From      => Check_Colony.World,
                     To        => World);
                  Update_Available_Commodity
                    (Available.Get_Available_Commodity_Reference)
                    .Set_Quantity (Available.Quantity - Supply)
                    .Done;
               end;
            end if;
         end;
      end loop;

      if Remaining = Zero then
         return;
      end if;

      declare
         Best_World : Harriet.Db.World_Reference :=
           Harriet.Db.Null_World_Reference;
         Best_Score : Non_Negative_Real := 0.0;
      begin
         for World_Knowledge of
           Harriet.Db.World_Knowledge.Select_By_Faction (Faction)
         loop
            if World_Knowledge.Deposits then
               declare
                  Deposit : constant Harriet.Db.Deposit.Deposit_Type :=
                    Harriet.Db.Deposit.Get_By_Deposit
                      (World_Knowledge.World, Resource);
               begin
                  if Deposit.Has_Element then
                     declare
                        This_Score : constant Non_Negative_Real :=
                          To_Real (Deposit.Available) * Deposit.Concentration;
                     begin
                        if This_Score > Best_Score then
                           Best_World := Deposit.World;
                           Best_Score := This_Score;
                        end if;
                     end;
                  end if;
               end;
            end if;
         end loop;

         declare
            use type Harriet.Db.World_Reference;
         begin
            if Best_World /= Harriet.Db.Null_World_Reference then
               declare
                  use Harriet.Db.Colonise_Goal;
                  Current_Goal : constant Colonise_Goal_Type :=
                    Get_By_Colonise_World_Goal (Faction, World);
               begin
                  if not Current_Goal.Has_Element then
                     Add_Colonisation_Goal
                       (Faction, Priority, World);
                  end if;
               end;
               Remaining := Zero;
            end if;
         end;
      end;

      for World_Knowledge of
        Harriet.Db.World_Knowledge.Select_By_Faction (Faction)
      loop
         if not World_Knowledge.Deposits then
            Add_World_Scan_Goal
              (Faction, Priority, World_Knowledge.World);
         end if;
      end loop;

      if Remaining > Zero then
         declare
            use Harriet.Db, Harriet.Db.Resource_Goal;
            Existing_Goal : constant Harriet.Db.Resource_Goal_Reference :=
              Get_Reference_By_Resource_Goal
                (Faction, World, Resource, Positive (Priority));
         begin
            if Existing_Goal = Null_Resource_Goal_Reference then
               Harriet.Db.Resource_Goal.Create
                 (Status   => Harriet.Db.Waiting,
                  Faction  => Faction,
                  Priority => Positive (Priority),
                  World    => World,
                  Resource => Resource,
                  Quantity => Remaining);
            else
               Harriet.Logging.Log
                 ("goals",
                  "replacing existing goal with "
                  & Show (Remaining));
               Update_Resource_Goal (Existing_Goal)
                 .Set_Quantity (Remaining)
                 .Done;
            end if;
         end;
      end if;

   end Colony_Needs_Resource;

   ---------------------------
   -- Add_Colonisation_Goal --
   ---------------------------

   procedure Add_Colonisation_Goal
     (Faction : Harriet.Db.Faction_Reference; Priority : Priority_Type;
      World   : Harriet.Db.World_Reference)
   is null;

end Harriet.Managers.Goals;
