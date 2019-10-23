with Harriet.Logging;

with Harriet.Worlds;

with Harriet.Db.Available_Commodity;
with Harriet.Db.Colony;
with Harriet.Db.Faction;
with Harriet.Db.Resource;
with Harriet.Db.Resource_Goal;
with Harriet.Db.Transport_Goal;

package body Harriet.Managers.Goals is

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
                    (Active    => True,
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

      if Remaining > Zero then
         declare
            use Harriet.Db, Harriet.Db.Resource_Goal;
            Existing_Goal : constant Harriet.Db.Resource_Goal_Reference :=
              Get_Reference_By_Resource_Goal (World, Resource);
         begin
            if Existing_Goal = Null_Resource_Goal_Reference then
               Harriet.Logging.Log
                 ("goals",
                  "replacing existing goal with "
                  & Show (Remaining));
               Harriet.Db.Resource_Goal.Create
                 (Active   => True,
                  Faction  => Faction,
                  Priority => Positive (Priority),
                  World    => World,
                  Resource => Resource,
                  Quantity => Remaining);
            else
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

   -------------------------
   -- Add_World_Scan_Goal --
   -------------------------

   procedure Add_World_Scan_Goal
     (Faction : Harriet.Db.Faction_Reference; Priority : Priority_Type;
      World   : Harriet.Db.World_Reference)
   is null;

end Harriet.Managers.Goals;
