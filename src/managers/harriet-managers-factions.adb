with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Quantities;
with Harriet.Real_Images;

with Harriet.Commodities.Maps;

with Harriet.Managers.Execution;

with Harriet.Db.Deposit;
with Harriet.Db.Faction;
with Harriet.Db.Resource;
with Harriet.Db.World;
with Harriet.Db.Commodity;
with Harriet.Db.Star_System;

package body Harriet.Managers.Factions is

   package Resource_Value_Maps is
     new Harriet.Commodities.Maps (Real);

   procedure Add_Resource
     (Map           : in out Resource_Value_Maps.Map;
      Resource      : Harriet.Db.Resource_Reference;
      Available     : Harriet.Quantities.Quantity_Type;
      Concentration : Unit_Real);

   type World_Record is
      record
         Reference : Harriet.Db.World_Reference;
         Resources : Resource_Value_Maps.Map;
      end record;

   package World_Record_Lists is
     new Ada.Containers.Doubly_Linked_Lists (World_Record);

   type Star_System_Record is
      record
         Star_System : Harriet.Db.Star_System_Reference;
         Worlds      : World_Record_Lists.List;
         Resources   : Resource_Value_Maps.Map;
      end record;

   package Star_System_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Star_System_Record);

   type Root_Faction_Manager is new Root_Manager_Type with
      record
         Faction        : Harriet.Db.Faction_Reference;
         Capital_System : Harriet.Db.Star_System_Reference;
         Capital_World  : Harriet.Db.World_Reference;
         Star_Systems   : Star_System_Lists.List;
      end record;

   overriding function Identifier
     (Manager : Root_Faction_Manager)
      return String
   is ("faction" & Harriet.Db.To_String (Manager.Faction) & " manager");

   overriding procedure Activate
     (Manager : not null access Root_Faction_Manager);

   procedure Check_Star_Systems
     (Manager : in out Root_Faction_Manager'Class);

   procedure Scan_System
     (Manager     : in out Root_Faction_Manager'Class;
      Star_System : Harriet.Db.Star_System_Reference);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Faction_Manager)
   is
   begin
      Manager.Check_Star_Systems;
      Manager.Set_Next_Update_Delay (Harriet.Calendar.Days (1));
   end Activate;

   ------------------
   -- Add_Resource --
   ------------------

   procedure Add_Resource
     (Map           : in out Resource_Value_Maps.Map;
      Resource      : Harriet.Db.Resource_Reference;
      Available     : Harriet.Quantities.Quantity_Type;
      Concentration : Unit_Real)
   is
      Commodity  : constant Harriet.Db.Commodity_Reference :=
        Harriet.Db.Resource.Get (Resource).Get_Commodity_Reference;
      This_Value : constant Non_Negative_Real :=
        Harriet.Quantities.To_Real (Available) * Concentration;

      procedure Update (Value : in out Real);

      ------------
      -- Update --
      ------------

      procedure Update (Value : in out Real) is
      begin
         Value := Value + This_Value;
      end Update;

   begin
      if not Map.Contains (Commodity) then
         Map.Insert (Commodity, 0.0);
      end if;
      Map.Update (Commodity, Update'Access);
   end Add_Resource;

   ------------------------
   -- Check_Star_Systems --
   ------------------------

   procedure Check_Star_Systems
     (Manager : in out Root_Faction_Manager'Class)
   is
   begin
      if Manager.Star_Systems.Is_Empty then
         Manager.Scan_System
           (Manager.Capital_System);
      end if;
   end Check_Star_Systems;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      Manager : Root_Faction_Manager;
      Faction : constant Harriet.Db.Faction.Faction_Type :=
        Harriet.Db.Faction.Get_Faction (Managed);
   begin
      Manager.Faction := Faction.Get_Faction_Reference;
      Manager.Capital_System := Faction.Capital_System;
      Manager.Capital_World := Faction.Capital_World;

      Harriet.Managers.Execution.Start_Middle_Manager
        (Faction => Manager.Faction,
         Area    => Fleet,
         Name    => Faction.Fleet_Manager);

      return new Root_Faction_Manager'(Manager);
   end Create_Default_Manager;

   -----------------
   -- Scan_System --
   -----------------

   procedure Scan_System
     (Manager     : in out Root_Faction_Manager'Class;
      Star_System : Harriet.Db.Star_System_Reference)
   is
      use type Harriet.Db.World_Reference;
      System_Rec : Star_System_Record :=
        Star_System_Record'
          (Star_System => Star_System,
           Worlds      => <>,
           Resources   => <>);

      Current_World : World_Record :=
        (Reference =>
           Harriet.Db.Null_World_Reference,
         others    => <>);

      procedure Log_Value
        (Commodity : Harriet.Db.Commodity_Reference;
         Value     : Real);

      procedure Update_Deposit
        (World         : Harriet.Db.World_Reference;
         Resource      : Harriet.Db.Resource_Reference;
         Availability  : Harriet.Quantities.Quantity_Type;
         Concentration : Unit_Real);

      ---------------
      -- Log_Value --
      ---------------

      procedure Log_Value
        (Commodity : Harriet.Db.Commodity_Reference;
         Value     : Real)
      is
      begin
         Manager.Log
           (Harriet.Db.Commodity.Get (Commodity).Tag
            & ": value "
            & Harriet.Real_Images.Approximate_Image
              (Value));
      end Log_Value;

      --------------------
      -- Update_Deposit --
      --------------------

      procedure Update_Deposit
        (World         : Harriet.Db.World_Reference;
         Resource      : Harriet.Db.Resource_Reference;
         Availability  : Harriet.Quantities.Quantity_Type;
         Concentration : Unit_Real)
      is
      begin
         if World /= Current_World.Reference then
            System_Rec.Worlds.Append (Current_World);
            Current_World := World_Record'
              (Reference => World,
               Resources => <>);
         end if;

         Add_Resource
           (Map           => Current_World.Resources,
            Resource      => Resource,
            Available     => Availability,
            Concentration => Concentration);
         Add_Resource
           (Map           => System_Rec.Resources,
            Resource      => Resource,
            Available     => Availability,
            Concentration => Concentration);
      end Update_Deposit;

   begin
      for World of Harriet.Db.World.Select_By_Star_System (Star_System) loop
         for Deposit of
           Harriet.Db.Deposit.Select_By_World
             (World.Get_World_Reference)
         loop
            Update_Deposit
              (World.Get_World_Reference,
               Deposit.Resource, Deposit.Available,
               Unit_Clamp (Deposit.Concentration));
         end loop;
      end loop;

      Manager.Log ("resources for "
                   & Harriet.Db.Star_System.Get (Star_System).Name);
      System_Rec.Resources.Iterate (Log_Value'Access);

      Manager.Star_Systems.Append (System_Rec);
   end Scan_System;

end Harriet.Managers.Factions;
