with Harriet.UI;

with Harriet.Logging;
with Harriet.Quantities;
with Harriet.Real_Images;

with Harriet.Worlds;

with Harriet.Db.Deposit;
with Harriet.Db.Deposit_Knowledge;
with Harriet.Db.Faction;
with Harriet.Db.Resource;
with Harriet.Db.World;
with Harriet.Db.World_Knowledge;

package body Harriet.Factions is

   --------------------
   -- Capital_System --
   --------------------

   function Capital_System
     (Faction : Harriet.Db.Faction_Reference)
      return Harriet.Db.Star_System_Reference
   is
   begin
      return Harriet.Db.World.Get (Capital_World (Faction)).Star_System;
   end Capital_System;

   --------------------
   -- Capital_System --
   --------------------

   function Capital_System
     (Faction : Faction_Type'Class)
      return Harriet.Db.Star_System_Reference
   is
   begin
      return Capital_System (Faction.Reference);
   end Capital_System;

   -------------------
   -- Capital_World --
   -------------------

   function Capital_World
     (Faction : Harriet.Db.Faction_Reference)
      return Harriet.Db.World_Reference
   is
   begin
      return Harriet.Db.Faction.Get (Faction).Capital_World;
   end Capital_World;

   -------------------
   -- Capital_World --
   -------------------

   function Capital_World
     (Faction : Faction_Type'Class)
      return Harriet.Db.World_Reference
   is
   begin
      return Capital_World (Faction.Reference);
   end Capital_World;

   -----------
   -- Color --
   -----------

   function Color
     (Faction : Faction_Type'Class)
      return Harriet.Color.Harriet_Color
   is
      Rec : constant Harriet.Db.Faction.Faction_Type :=
              Harriet.Db.Faction.Get (Faction.Reference);
   begin
      return (Rec.Red, Rec.Green, Rec.Blue, 1.0);
   end Color;

   -----------------------------
   -- Discover_World_Deposits --
   -----------------------------

   procedure Discover_World_Deposits
     (Faction : Harriet.Db.Faction_Reference;
      World   : Harriet.Db.World_Reference;
      Minimum : Unit_Real)
   is
      Has_Knowledge : constant Harriet.Db.Has_Knowledge_Reference :=
        Harriet.Db.Faction.Get (Faction).Get_Has_Knowledge_Reference;
   begin
      if not Harriet.Db.World_Knowledge.Is_World_Knowledge
        (Faction, World)
      then
         Harriet.Db.World_Knowledge.Create
           (Faction        => Faction,
            Has_Knowledge  => Has_Knowledge,
            Knowable       =>
              Harriet.Db.World.Get (World).Get_Knowable_Reference,
            Existence      => True,
            Current        => True,
            World          => World,
            Classification => True,
            Orbit          => True,
            Deposits       => False);
      end if;

      declare
         Ref : constant Harriet.Db.World_Knowledge_Reference :=
           Harriet.Db.World_Knowledge.Get_Reference_By_World_Knowledge
             (Faction, World);
      begin
         Harriet.Db.World_Knowledge.Update_World_Knowledge (Ref)
           .Set_Deposits (True)
           .Done;
      end;

      for Deposit of
        Harriet.Db.Deposit.Select_By_World (World)
      loop
         if not Harriet.Db.Deposit_Knowledge.Is_Deposit_Knowledge
           (Faction, Deposit.Get_Deposit_Reference)
         then
            if Deposit.Concentration >= Minimum then
               Harriet.Logging.Log
                 (Harriet.Db.Faction.Get (Faction).Name,
                  Harriet.Worlds.Name (World)
                  & ": discovers "
                  & Harriet.Quantities.Show (Deposit.Available)
                  & " "
                  & Harriet.Db.Resource.Get (Deposit.Resource).Tag
                  & " at "
                  & Harriet.Real_Images.Approximate_Image
                    (Deposit.Concentration * 100.0)
                  & "%");

               Harriet.Db.Deposit_Knowledge.Create
                 (Faction        => Faction,
                  Has_Knowledge  => Has_Knowledge,
                  Knowable       => Deposit.Get_Knowable_Reference,
                  Existence      => True,
                  Current        => True,
                  Resource       => Deposit.Resource,
                  Deposit        => Deposit.Get_Deposit_Reference);

            else
               Harriet.Logging.Log
                 (Harriet.Db.Faction.Get (Faction).Name,
                  Harriet.Worlds.Name (World)
                  & ": misses "
                  & Harriet.Quantities.Show (Deposit.Available)
                  & " "
                  & Harriet.Db.Resource.Get (Deposit.Resource).Tag
                  & " at "
                  & Harriet.Real_Images.Approximate_Image
                    (Deposit.Concentration * 100.0)
                  & "%");
            end if;
         end if;
      end loop;

   end Discover_World_Deposits;

   ---------
   -- Get --
   ---------

   function Get
     (Reference : Harriet.Db.Faction_Reference)
      return Faction_Type'Class
   is
   begin
      return Faction_Type'(Reference => Reference);
   end Get;

   ----------------------
   -- Get_User_Faction --
   ----------------------

   function Get_User_Faction
     (Reference : Harriet.Db.User_Reference)
      return Faction_Type'Class
   is
   begin
      return Get
        (Harriet.Db.Faction.First_Reference_By_User (Reference));
   end Get_User_Faction;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Faction : Faction_Type'Class)
      return Boolean
   is
      use type Harriet.Db.Faction_Reference;
   begin
      return Faction.Reference /= Harriet.Db.Null_Faction_Reference;
   end Has_Element;

   ----------
   -- Name --
   ----------

   function Name
     (Faction : Faction_Type'Class)
      return String
   is
   begin
      return Harriet.Db.Faction.Get (Faction.Reference).Name;
   end Name;

   ----------
   -- Name --
   ----------

   function Name
     (Faction : Harriet.Db.Faction_Reference)
      return String
   is
   begin
      return Harriet.Db.Faction.Get (Faction).Name;
   end Name;

   -----------------
   -- Synchronise --
   -----------------

   procedure Synchronise (Faction : Harriet.Db.Faction_Reference) is
   begin
      Harriet.UI.Send_Message
        (Harriet.UI.New_Message (Faction)
         .Cash_Changed);
   end Synchronise;

end Harriet.Factions;
