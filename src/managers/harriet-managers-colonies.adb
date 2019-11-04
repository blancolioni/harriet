with Ada.Containers.Vectors;

with Harriet.Money;
with Harriet.Quantities;
with Harriet.Real_Images;

with Harriet.Colonies;
with Harriet.Commodities;
with Harriet.Factions;
with Harriet.Installations;
with Harriet.Ships;
with Harriet.Worlds;

with Harriet.Managers.Colonies.Requirements;
with Harriet.Managers.Goals;

with Harriet.Db.Colony;
with Harriet.Db.Commodity;
with Harriet.Db.Consumer_Commodity;
--  with Harriet.Db.Deposit;
with Harriet.Db.Expense;
with Harriet.Db.Facility;
with Harriet.Db.Facility_Input;
with Harriet.Db.Installation;
--  with Harriet.Db.Manufactured;
with Harriet.Db.Mining_Facility;
with Harriet.Db.Production_Goal;
--  with Harriet.Db.Resource;
with Harriet.Db.Revenue;
with Harriet.Db.Ship;
with Harriet.Db.Stock_Item;

package body Harriet.Managers.Colonies is

   type Root_Colony_Manager is new Root_Manager_Type with
      record
         Colony_Has_Stock      : Harriet.Db.Has_Stock_Reference;
         Colony                : Harriet.Db.Colony_Reference;
         Faction               : Harriet.Db.Faction_Reference;
         World                 : Harriet.Db.World_Reference;
         Total_Pop             : Harriet.Quantities.Quantity_Type;
         Working_Pop           : Harriet.Quantities.Quantity_Type;
         Idle_Pop              : Harriet.Quantities.Quantity_Type;
         Missing_Pop           : Harriet.Quantities.Quantity_Type;
         Require               : Requirements.Colony_Requirements;
         Available_Mines       : Natural := 0;
         Available_Strip_Mines : Natural := 0;
         Available_Industry    : Natural := 0;
         Required_Mines        : Natural := 0;
         Required_Strip_Mines  : Natural := 0;
         Required_Industry     : Natural := 0;
         Available_Power       : Non_Negative_Real := 0.0;
         Required_Power        : Non_Negative_Real := 0.0;
      end record;

   overriding function Identifier
     (Manager : Root_Colony_Manager)
      return String
   is ("colony" & Harriet.Db.To_String (Manager.Colony) & " manager");

   overriding procedure Activate
     (Manager : not null access Root_Colony_Manager);

   procedure Check_Builds
     (Manager : in out Root_Colony_Manager'Class);

   procedure Check_Installations
     (Manager : in out Root_Colony_Manager'Class);

   procedure Check_Population
     (Manager : in out Root_Colony_Manager'Class);

   procedure Check_Ships
     (Manager : in out Root_Colony_Manager'Class);

   procedure Check_Revenue
     (Manager : Root_Colony_Manager'Class);

   procedure Elaborate_Requirements
     (Manager : in out Root_Colony_Manager'Class);

   procedure Process_Requirements
     (Manager : in out Root_Colony_Manager'Class);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Colony_Manager)
   is
   begin
      Manager.Log
        (Harriet.Factions.Name (Harriet.Factions.Get (Manager.Faction))
         & " colony on "
         & Harriet.Worlds.Name (Manager.World)
         & " activating");

      for Stock_Item of
        Harriet.Db.Stock_Item.Select_By_Has_Stock
          (Manager.Colony_Has_Stock)
      loop
         Manager.Log
           (Harriet.Db.Commodity.Get (Stock_Item.Commodity).Tag
            & ": " & Harriet.Quantities.Show (Stock_Item.Quantity));
      end loop;

      Requirements.Clear_Requirements (Manager.Require);

      for Goal of
        Harriet.Db.Production_Goal.Select_By_Colony
          (Manager.Colony)
      loop
         Harriet.Db.Production_Goal.Update_Production_Goal
           (Goal.Get_Production_Goal_Reference)
           .Set_Quantity (Harriet.Quantities.Zero)
           .Done;
      end loop;

      Manager.Check_Population;
      Manager.Check_Installations;
      Manager.Check_Ships;
      Manager.Check_Builds;
      Manager.Check_Revenue;

      Manager.Elaborate_Requirements;
      Manager.Process_Requirements;

      Manager.Set_Next_Update_Delay (Harriet.Calendar.Days (1));
   end Activate;

   ------------------
   -- Check_Builds --
   ------------------

   procedure Check_Builds
     (Manager : in out Root_Colony_Manager'Class)
   is
      use Harriet.Quantities;
   begin
      if Manager.Idle_Pop = Zero then
         return;
      end if;

   end Check_Builds;

   -------------------------
   -- Check_Installations --
   -------------------------

   procedure Check_Installations
     (Manager : in out Root_Colony_Manager'Class)
   is
      use Harriet.Quantities;
      Colony : constant Harriet.Colonies.Colony_Handle :=
        Harriet.Colonies.Get (Manager.Colony);
   begin
      Manager.Total_Pop := Colony.Population;
      Manager.Working_Pop := Zero;

      for Installation of
        Harriet.Db.Installation.Select_By_Colony
          (Manager.Colony)
      loop
         declare
            Facility : constant Harriet.Db.Facility.Facility_Type :=
              Harriet.Db.Facility.Get (Installation.Facility);
         begin
            if Facility.Generate > 0 then
               Manager.Available_Power :=
                 Manager.Available_Power + Real (Facility.Generate);
            else
               Manager.Required_Power :=
                 Manager.Required_Power + Real (Facility.Power);
            end if;

            for Input of
              Harriet.Db.Facility_Input.Select_By_Facility
                (Installation.Facility)
            loop
               Requirements.Add_Requirement
                 (Requirements => Manager.Require,
                  Commodity    => Input.Commodity,
                  Quantity     => Input.Quantity,
                  Priority     =>
                    (if Facility.Generate > 0
                     then High_Priority - 1
                     else High_Priority));
            end loop;

            Manager.Working_Pop :=
              Manager.Working_Pop
                + Harriet.Db.Facility.Get (Installation.Facility).Employees;
         end;
      end loop;

      Manager.Idle_Pop :=
        (if Manager.Working_Pop >= Manager.Total_Pop
         then Zero else Manager.Total_Pop - Manager.Working_Pop);
      Manager.Missing_Pop :=
        (if Manager.Working_Pop <= Manager.Total_Pop
         then Zero else Manager.Working_Pop - Manager.Total_Pop);

      if Manager.Missing_Pop > Scale (Manager.Total_Pop, 0.1) then
         Requirements.Add_Population_Requirement
           (Manager.Require, Manager.Missing_Pop, Low_Priority);
      end if;

   end Check_Installations;

   ----------------------
   -- Check_Population --
   ----------------------

   procedure Check_Population
     (Manager : in out Root_Colony_Manager'Class)
   is
      use Harriet.Quantities;
      Current_Pop : constant Quantity_Type :=
        Harriet.Db.Colony.Get (Manager.Colony).Population;
   begin
      for Consumer_Commodity of
        Harriet.Db.Consumer_Commodity.Scan_By_Commodity
      loop
         if Consumer_Commodity.Pop_Per_Item < Current_Pop then
            Requirements.Add_Requirement
              (Requirements => Manager.Require,
               Commodity    => Consumer_Commodity.Commodity,
               Quantity     =>
                 Scale (Current_Pop / Consumer_Commodity.Pop_Per_Item, 10.0),
               Priority     => Medium_Priority);
         end if;
      end loop;
   end Check_Population;

   -------------------
   -- Check_Revenue --
   -------------------

   procedure Check_Revenue
     (Manager : Root_Colony_Manager'Class)
   is
      use Harriet.Calendar;
      use Harriet.Money;
      use Harriet.Real_Images;
      Now : constant Time := Clock;
      Last_Revenue : Money_Type := Zero;
      Last_Expense : Money_Type := Zero;
      Tax_Rate     : constant Unit_Real :=
        Harriet.Db.Colony.Get (Manager.Colony).Tax_Rate;
   begin
      for Revenue of
        Harriet.Db.Revenue.Select_Historical_Revenue_Bounded_By_Date
          (Colony      => Manager.Colony,
           Start_Date  => Now - Days (2),
           Finish_Date => Now)
      loop
         Last_Revenue := Revenue.Revenue;
      end loop;

      for Expense of
        Harriet.Db.Expense.Select_Historical_Expense_Bounded_By_Date
          (Colony      => Manager.Colony,
           Start_Date  => Now - Days (2),
           Finish_Date => Now)
      loop
         Last_Expense := Expense.Expense;
      end loop;

      if Last_Expense > Zero and then Last_Revenue > Zero then
         declare
            New_Tax_Rate : Unit_Real := Tax_Rate;
         begin
            if Last_Revenue > Last_Expense then
               New_Tax_Rate :=
                 Tax_Rate * To_Real (Last_Expense) / To_Real (Last_Revenue);
            elsif Last_Revenue < Last_Expense then
               New_Tax_Rate :=
                 Real'Min
                   (0.6,
                    1.02 * Tax_Rate * To_Real (Last_Expense)
                    / To_Real (Last_Revenue));
            end if;

            if New_Tax_Rate /= Tax_Rate
              and then (Tax_Rate = 0.0
                        or else abs (New_Tax_Rate - Tax_Rate) / Tax_Rate
                        > 0.01)
            then
               Manager.Log ("change tax rate from "
                            & Approximate_Image (Tax_Rate * 100.0)
                            & "%"
                            & " to "
                            & Approximate_Image (New_Tax_Rate * 100.0)
                            & "%");
               Harriet.Db.Colony.Update_Colony (Manager.Colony)
                 .Set_Tax_Rate (New_Tax_Rate)
                 .Done;
            end if;
         end;
      end if;
   end Check_Revenue;

   -----------------
   -- Check_Ships --
   -----------------

   procedure Check_Ships
     (Manager : in out Root_Colony_Manager'Class)
   is
      Required_Fuel : Non_Negative_Real := 0.0;
      Commodity     : constant Harriet.Db.Commodity_Reference :=
        Harriet.Db.Commodity.Get_Reference_By_Tag
          ("ship-fuel");
   begin
      for Ship of
        Harriet.Db.Ship.Select_By_Home (Manager.World)
      loop
         Required_Fuel :=
           Required_Fuel + Harriet.Ships.Tank_Size
           (Harriet.Ships.Get (Ship.Get_Ship_Reference));
      end loop;

      Requirements.Add_Requirement
        (Requirements => Manager.Require,
         Commodity    => Commodity,
         Quantity     =>
           Harriet.Quantities.To_Quantity (Required_Fuel / 4.0),
         Priority     => High_Priority);

      Requirements.Add_Requirement
        (Requirements => Manager.Require,
         Commodity    => Commodity,
         Quantity     =>
           Harriet.Quantities.To_Quantity (2.0 * Required_Fuel),
         Priority     => Low_Priority);

   end Check_Ships;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      Colony  : constant Harriet.Db.Colony.Colony_Type :=
                  Harriet.Db.Colony.Get_Colony (Managed);
      Manager : Root_Colony_Manager :=
        Root_Colony_Manager'
          (Root_Manager_Type with
           Colony           => Colony.Get_Colony_Reference,
           Faction          => Colony.Faction,
           World            => Colony.World,
           Colony_Has_Stock => Colony.Get_Has_Stock_Reference,
           Total_Pop        => Colony.Population,
           Working_Pop      => Harriet.Quantities.Zero,
           Idle_Pop         => Harriet.Quantities.Zero,
           Missing_Pop      => Harriet.Quantities.Zero,
           others           => <>);
   begin
      Manager.Colony := Colony.Get_Colony_Reference;
      return new Root_Colony_Manager'(Manager);
   end Create_Default_Manager;

   ----------------------------
   -- Elaborate_Requirements --
   ----------------------------

   procedure Elaborate_Requirements
     (Manager : in out Root_Colony_Manager'Class)
   is

      Elaborated_Requirements : Requirements.Colony_Requirements :=
        Manager.Require;

      Committed_Stock    : Harriet.Commodities.Stock_Type;

      procedure Add_Ingredients
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Quantities.Quantity_Type;
         Priority  : Priority_Type);

      procedure Elaborate_Commodity
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Quantities.Quantity_Type;
         Priority  : Priority_Type);

      procedure Elaborate_Facility
        (Facility  : Harriet.Db.Facility_Reference;
         Count     : Positive;
         Priority  : Priority_Type);

      ---------------------
      -- Add_Ingredients --
      ---------------------

      procedure Add_Ingredients
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Quantities.Quantity_Type;
         Priority  : Priority_Type)
      is
         procedure Add_Ingredient
           (Ingredient : Harriet.Db.Commodity_Reference;
            In_Quantity : Quantities.Quantity_Type);

         --------------------
         -- Add_Ingredient --
         --------------------

         procedure Add_Ingredient
           (Ingredient  : Harriet.Db.Commodity_Reference;
            In_Quantity : Quantities.Quantity_Type)
         is
            use Harriet.Quantities;
         begin
            Elaborate_Commodity (Ingredient, In_Quantity * Quantity, Priority);
         end Add_Ingredient;

      begin
         Harriet.Commodities.Scan_Ingredients
           (Commodity,
            Add_Ingredient'Access);
      end Add_Ingredients;

      -------------------------
      -- Elaborate_Commodity --
      -------------------------

      procedure Elaborate_Commodity
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Quantities.Quantity_Type;
         Priority  : Priority_Type)
      is
      begin
         Requirements.Add_Requirement
           (Requirements => Elaborated_Requirements,
            Commodity    => Commodity,
            Quantity     => Quantity,
            Priority     => Priority);

         if Harriet.Commodities.Is_Resource (Commodity) then
            Manager.Log
              ("resource requirement: "
               & Harriet.Quantities.Show (Quantity)
               & " " & Harriet.Db.Commodity.Get (Commodity).Tag
               & " priority" & Priority'Image);
         else
            declare
               use Harriet.Quantities;
               Current : constant Quantity_Type :=
                 Harriet.Commodities.Current_Quantity
                   (Of_Stock  => Manager.Colony_Has_Stock,
                    Commodity => Commodity);
               Committed : constant Quantity_Type :=
                 Harriet.Commodities.Current_Quantity
                   (Committed_Stock, Commodity);
            begin
               Manager.Log
                 ("commodity requirement: "
                  & Harriet.Db.Commodity.Get (Commodity).Tag
                  & ": required: " & Harriet.Quantities.Show (Quantity)
                  & "; have: " & Harriet.Quantities.Show (Current)
                  & "; committed: " & Harriet.Quantities.Show (Committed)
                  & "; available: " & Harriet.Quantities.Show
                    (Current - Min (Current, Committed)));
               if Committed + Quantity > Current then
                  Add_Ingredients
                    (Commodity,
                     Min (Quantity, Committed + Quantity - Current),
                     Priority);
               end if;
            end;
         end if;
      end Elaborate_Commodity;

      ------------------------
      -- Elaborate_Facility --
      ------------------------

      procedure Elaborate_Facility
        (Facility  : Harriet.Db.Facility_Reference;
         Count     : Positive;
         Priority  : Priority_Type)
        is
      begin
         null;
      end Elaborate_Facility;

   begin
      Requirements.Scan_Facility_Requirements
        (Manager.Require, Elaborate_Facility'Access);
      Requirements.Scan_Commodity_Requirements
        (Manager.Require, Elaborate_Commodity'Access);
      Manager.Require := Elaborated_Requirements;
   end Elaborate_Requirements;

   --------------------------
   -- Process_Requirements --
   --------------------------

   procedure Process_Requirements
     (Manager : in out Root_Colony_Manager'Class)
   is

      package Installation_Vectors is new
        Ada.Containers.Vectors (Positive, Harriet.Db.Installation_Reference,
                                Harriet.Db."=");

      Mine_Map       : Installation_Vectors.Vector;
      Strip_Mine_Map : Installation_Vectors.Vector;
      Industry_Map   : Installation_Vectors.Vector;

      function Active_Mines
        (Resource : Harriet.Db.Resource_Reference)
         return Natural;

      procedure Assign_Mines
        (Commodity   : Harriet.Db.Commodity_Reference;
         Requirement : Harriet.Quantities.Quantity_Type;
         Priority    : Priority_Type)
      is null;

      procedure Add_Production_Goal
        (Commodity   : Harriet.Db.Commodity_Reference;
         Requirement : Harriet.Quantities.Quantity_Type;
         Priority    : Priority_Type);

      procedure Initialize_Installation_Maps;

      ------------------
      -- Active_Mines --
      ------------------

      function Active_Mines
        (Resource : Harriet.Db.Resource_Reference)
         return Natural
      is
         Mine : constant Harriet.Db.Mining_Facility.Mining_Facility_Type :=
           Harriet.Db.Mining_Facility.First_By_Resource
             (Resource);
         Facility : constant Harriet.Db.Facility_Reference :=
           Mine.Get_Facility_Reference;
      begin
         return Result : Natural := 0 do
            for Installation of
              Harriet.Db.Installation.Select_By_World_Facility
                (Manager.World, Facility)
            loop
               Result := Result + 1;
            end loop;
         end return;
      end Active_Mines;

      -------------------------
      -- Add_Production_Goal --
      -------------------------

      procedure Add_Production_Goal
        (Commodity   : Harriet.Db.Commodity_Reference;
         Requirement : Harriet.Quantities.Quantity_Type;
         Priority    : Priority_Type)
      is
      begin
         if Harriet.Commodities.Is_Resource (Commodity)
           and then Active_Mines
             (Harriet.Commodities.To_Resource (Commodity))
               > 0
         then
            Assign_Mines (Commodity, Requirement, Priority);
         else
            declare
               use Harriet.Quantities;
               Available      : constant Quantity_Type :=
                 Harriet.Commodities.Current_Quantity
                   (Manager.Colony_Has_Stock, Commodity);
               Quantity       : constant Quantity_Type :=
                 (if Requirement > Available
                  then Requirement - Available
                  else Zero);
            begin
               Manager.Log
                 (Harriet.Db.Commodity.Get (Commodity).Tag
                  & ": requirement: "
                  & Show (Requirement)
                  & "; available: "
                  & Show (Available)
                  & "; missing: "
                  & Show (Quantity));

               if Quantity > Zero then
                  if Harriet.Commodities.Is_Resource (Commodity) then
                     Harriet.Managers.Goals.Colony_Needs_Resource
                       (Faction  => Manager.Faction,
                        Priority => Priority,
                        Colony   => Manager.Colony,
                        Resource =>
                          Harriet.Commodities.To_Resource (Commodity),
                        Quantity => Quantity);
                  end if;

                  Harriet.Installations.Add_Production_Goal
                    (Colony    => Manager.Colony,
                     Commodity => Commodity,
                     Quantity  => Quantity,
                     Priority  => Priority);
               end if;
            end;
         end if;
      end Add_Production_Goal;

      ------------------
      -- Assign_Mines --
      ------------------

--        procedure Assign_Mines
--          (Commodity    : Harriet.Db.Commodity_Reference;
--           Requirement  : Harriet.Quantities.Quantity_Type;
--           Priority     : Priority_Type)
--        is
--           use Harriet.Quantities;
--           Resource : constant Harriet.Db.Resource_Reference :=
--        Harriet.Db.Resource.Get_Resource (Commodity).Get_Resource_Reference;
--           Deposit : constant Harriet.Db.Deposit.Deposit_Type :=
--             Harriet.Db.Deposit.Get_By_Deposit
--               (Manager.World, Resource);
--       Mined_Resource : constant Harriet.Db.Resource_Reference := Resource;
--           Available      : constant Quantity_Type :=
--             Harriet.Commodities.Current_Quantity
--               (Manager.Colony_Has_Stock, Commodity);
--           Quantity : constant Quantity_Type :=
--             (if Requirement > Available
--              then Requirement - Available
--              else Zero);
--        begin
--
--           Manager.Log
--             (Harriet.Db.Commodity.Get (Commodity).Tag
--              & ": requirement: "
--              & Show (Requirement)
--              & "; available: "
--              & Show (Available)
--              & "; missing: "
--              & Show (Quantity));
--
--           if Quantity > Zero then
--              if Deposit.Has_Element
--                and then Deposit.Available > Zero
--                and then Deposit.Concentration > 0.0
--              then
--                 Manager.Log
--                   ("found " & Harriet.Db.Commodity.Get (Commodity).Tag
--                    & " deposit: size "
--                    & Show (Deposit.Available)
--                    & " concentration "
--                    & Harriet.Real_Images.Approximate_Image
--                      (Deposit.Concentration * 100.0)
--                    & "%");
--
--                 declare
--               Available     : constant Quantity_Type := Deposit.Available;
--               Concentration : constant Unit_Real := Deposit.Concentration;
--                    Estimated_Mine : constant Real :=
--                      To_Real (Available) * Concentration / 1.0e4;
--                    Required      : constant Natural :=
--                      Natural (To_Real (Quantity) / Estimated_Mine) + 1;
--                    Assign        : constant Natural :=
--                      Natural'Min (Required, Remaining_Mines);
--                 begin
--                    if Required > Remaining_Mines then
--                       Manager.Required_Mines := Manager.Required_Mines
--                         + Required - Remaining_Mines;
--                    end if;
--
--                    for I in 1 .. Assign loop
--                       Next_Mine := Next_Mine + 1;
--                       Harriet.Db.Installation.Update_Installation
--                         (Mine_Map (Next_Mine))
--                           .Set_Resource (Mined_Resource)
--                         .Done;
--                    end loop;
--                    Remaining_Mines := Remaining_Mines - Assign;
--
--                    Manager.Log
--                      (Show (Quantity) & " "
--                       & Harriet.Db.Commodity.Get (Commodity).Tag
--                       & " requires"
--                       & Required'Image & " mines");
--
--                 end;
--              else
--
--                 Harriet.Managers.Goals.Colony_Needs_Resource
--                   (Faction  => Manager.Faction,
--                    Priority => Priority,
--                    Colony   => Manager.Colony,
--                    Resource => Resource,
--                    Quantity => Quantity);
--
--                 Harriet.Installations.Add_Production_Goal
--                   (Colony    => Manager.Colony,
--                    Commodity => Commodity,
--                    Quantity  => Requirement,
--                    Priority  => Priority);
--
--              end if;
--
--           end if;
--
--        end Assign_Mines;

      ----------------------------------
      -- Initialize_Installation_Maps --
      ----------------------------------

      procedure Initialize_Installation_Maps is
      begin

         Manager.Available_Mines := 0;
         Manager.Available_Strip_Mines := 0;
         Manager.Available_Industry := 0;

         for Installation of
           Harriet.Db.Installation.Select_By_Colony
             (Manager.Colony)
         loop
            declare
               Facility : constant Harriet.Db.Facility.Facility_Type :=
                 Harriet.Db.Facility.Get (Installation.Facility);
            begin
               if Facility.Mining > 0 then
                  Manager.Available_Mines :=
                    Manager.Available_Mines + 1;
                  Mine_Map.Append (Installation.Get_Installation_Reference);
               elsif Facility.Strip_Mining > 0 then
                  Manager.Available_Strip_Mines :=
                    Manager.Available_Strip_Mines + 1;
                  Strip_Mine_Map.Append
                    (Installation.Get_Installation_Reference);
               elsif Facility.Industry > 0 then
                  Manager.Available_Industry := Manager.Available_Industry + 1;
                  Industry_Map.Append
                    (Installation.Get_Installation_Reference);
               end if;
            end;
         end loop;

--           Remaining_Mines := Manager.Available_Mines;
--
--           Manager.Log
--             ("found" & Manager.Available_Mines'Image & " mines and"
--              & Manager.Available_Industry'Image & " industry");
      end Initialize_Installation_Maps;

   begin
      Initialize_Installation_Maps;
      Requirements.Scan_Commodity_Requirements
        (Manager.Require, Add_Production_Goal'Access);

   end Process_Requirements;

end Harriet.Managers.Colonies;
