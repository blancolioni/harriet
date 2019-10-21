with Ada.Containers.Vectors;

with Harriet.Money;
with Harriet.Quantities;
with Harriet.Real_Images;

with Harriet.Colonies;
with Harriet.Commodities;
with Harriet.Factions;
with Harriet.Worlds;

with Harriet.Managers.Colonies.Requirements;

with Harriet.Db.Colony;
with Harriet.Db.Commodity;
with Harriet.Db.Deposit;
with Harriet.Db.Expense;
with Harriet.Db.Facility;
with Harriet.Db.Facility_Input;
with Harriet.Db.Installation;
with Harriet.Db.Resource;
with Harriet.Db.Revenue;

package body Harriet.Managers.Colonies is

   type Root_Colony_Manager is new Root_Manager_Type with
      record
         Colony_Has_Stock   : Harriet.Db.Has_Stock_Reference;
         Colony             : Harriet.Db.Colony_Reference;
         Faction            : Harriet.Db.Faction_Reference;
         World              : Harriet.Db.World_Reference;
         Total_Pop          : Harriet.Quantities.Quantity_Type;
         Working_Pop        : Harriet.Quantities.Quantity_Type;
         Idle_Pop           : Harriet.Quantities.Quantity_Type;
         Missing_Pop        : Harriet.Quantities.Quantity_Type;
         Require            : Requirements.Colony_Requirements;
         Resources          : Requirements.Colony_Requirements;
         Available_Mines    : Natural := 0;
         Available_Strip_Mines    : Natural := 0;
         Available_Industry : Natural := 0;
         Required_Mines     : Natural := 0;
         Required_Strip_Mines : Natural := 0;
         Required_Industry  : Natural := 0;
         Available_Power    : Non_Negative_Real := 0.0;
         Required_Power     : Non_Negative_Real := 0.0;
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

      Requirements.Clear_Requirements (Manager.Require);

      Manager.Check_Installations;
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
                  Priority     => Medium_Prority);
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
           (Manager.Require, Manager.Missing_Pop, Medium_Prority);
      end if;

   end Check_Installations;

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
                    Tax_Rate * To_Real (Last_Expense)
                    / To_Real (Last_Revenue));
            end if;

            if New_Tax_Rate /= Tax_Rate then
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
                     Colony          => Colony.Get_Colony_Reference,
                     Faction         => Colony.Faction,
                     World           => Colony.World,
                     Total_Pop       => Colony.Population,
                     Working_Pop     => Harriet.Quantities.Zero,
                     Idle_Pop        => Harriet.Quantities.Zero,
                     Missing_Pop     => Harriet.Quantities.Zero,
                     others          => <>);
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

      Resource_Requirements : Requirements.Colony_Requirements;
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
         if Harriet.Commodities.Is_Resource (Commodity) then
            Manager.Log
              ("resource requirement: "
               & Harriet.Quantities.Show (Quantity)
               & " " & Harriet.Db.Commodity.Get (Commodity).Tag
               & " priority" & Priority'Image);

            Requirements.Add_Requirement
              (Requirements => Resource_Requirements,
               Commodity    => Commodity,
               Quantity     => Quantity,
               Priority     => Priority);
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
      Manager.Resources := Resource_Requirements;
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

      Remaining_Mines       : Natural := 0;
      Remaining_Strip_Mines : Natural := 0;
      Remaining_Industry    : Natural := 0;

      Mine_Map       : Installation_Vectors.Vector;
      Strip_Mine_Map : Installation_Vectors.Vector;
      Industry_Map   : Installation_Vectors.Vector;

      Next_Mine       : Natural := 0;
      Next_Strip_Mine : Natural := 0;
      Next_Industry   : Natural := 0;

      procedure Assign_Mines
        (Commodity   : Harriet.Db.Commodity_Reference;
         Requirement : Harriet.Quantities.Quantity_Type;
         Priority    : Priority_Type);

      procedure Initialize_Installation_Maps;

      ------------------
      -- Assign_Mines --
      ------------------

      procedure Assign_Mines
        (Commodity    : Harriet.Db.Commodity_Reference;
         Requirement  : Harriet.Quantities.Quantity_Type;
         Priority     : Priority_Type)
      is
         pragma Unreferenced (Priority);
         use Harriet.Quantities;
         Resource : constant Harriet.Db.Resource_Reference :=
           Harriet.Db.Resource.Get_Resource (Commodity).Get_Resource_Reference;
         Deposit : constant Harriet.Db.Deposit.Deposit_Type :=
           Harriet.Db.Deposit.Get_By_Deposit
             (Manager.World, Resource);
         Mined_Resource : constant Harriet.Db.Resource_Reference := Resource;
         Available      : constant Quantity_Type :=
           Harriet.Commodities.Current_Quantity
             (Manager.Colony_Has_Stock, Commodity);
         Quantity : constant Quantity_Type :=
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
            if Deposit.Has_Element
              and then Deposit.Available > Zero
              and then Deposit.Concentration > 0.0
            then
               Manager.Log
                 ("found " & Harriet.Db.Commodity.Get (Commodity).Tag
                  & " deposit: size "
                  & Image (Deposit.Available)
                  & " concentration "
                  & Harriet.Real_Images.Approximate_Image
                    (Deposit.Concentration * 100.0)
                  & "%");

               declare
                  Available     : constant Quantity_Type := Deposit.Available;
                  Concentration : constant Unit_Real := Deposit.Concentration;
                  Required      : constant Natural :=
                    Natural (To_Real (Quantity)
                             / (1.0e4
                               / To_Real (Available) * Concentration));
                  Assign        : constant Natural :=
                    Natural'Min (Required, Remaining_Mines);
               begin
                  if Required > Remaining_Mines then
                     Manager.Required_Mines := Manager.Required_Mines
                       + Required - Remaining_Mines;
                  end if;

                  for I in 1 .. Assign loop
                     Next_Mine := Next_Mine + 1;
                     Harriet.Db.Installation.Update_Installation
                       (Mine_Map (Next_Mine))
                         .Set_Resource (Mined_Resource)
                       .Done;
                  end loop;
                  Remaining_Mines := Remaining_Mines - Assign;

                  Manager.Log
                    (Show (Quantity) & " "
                     & Harriet.Db.Commodity.Get (Commodity).Tag
                     & " requires"
                     & Required'Image & " mines");

               end;
            else
               Manager.Log ("converting raw resources to "
                            & Show (Quantity)
                            & " "
                            & Harriet.Db.Commodity.Get (Commodity).Tag);
               declare
                  Current_Raw : constant Quantity_Type :=
                    Harriet.Commodities.Current_Quantity
                      (Manager.Colony_Has_Stock,
                       Harriet.Commodities.Raw_Resources);
                  Required_Raw : constant Quantity_Type :=
                    Scale (Quantity, 10.0);
                  Missing_Raw  : constant Quantity_Type :=
                    Required_Raw - Min (Current_Raw, Required_Raw);
                  Required_Industry : constant Natural :=
                    (Natural (To_Real (Quantity)) + 99) / 10;
                  Assigned_Industry : constant Natural :=
                    Natural'Min (Required_Industry, Remaining_Industry);
                  Required_Strip_Mines : constant Natural :=
                    (Natural (To_Real (Missing_Raw) * 10.0) + 99) / 100;
                  Assigned_Strip_Mines : constant Natural :=
                    Natural'Min (Required_Strip_Mines, Remaining_Strip_Mines);
               begin

                  if Required_Strip_Mines > Remaining_Strip_Mines then
                     Manager.Required_Strip_Mines :=
                       Manager.Required_Strip_Mines
                       + Required_Strip_Mines - Remaining_Strip_Mines;
                  end if;

                  for I in 1 .. Assigned_Strip_Mines loop
                     Next_Strip_Mine := Next_Strip_Mine + 1;
                  end loop;

                  Remaining_Strip_Mines := Remaining_Strip_Mines
                    - Assigned_Strip_Mines;

                  Manager.Log
                    (Show (Quantity) & " "
                     & Harriet.Db.Commodity.Get (Commodity).Tag
                     & " requires"
                     & Required_Strip_Mines'Image & " strip mines");

                  Manager.Log
                    ("assigning" & Assigned_Industry'Image
                     & " industry");
                  if Assigned_Industry < Required_Industry then
                     Manager.Required_Industry := Manager.Required_Industry
                       + Required_Industry - Assigned_Industry;
                  end if;

                  for I in 1 .. Assigned_Industry loop
                     Next_Industry := Next_Industry + 1;
                     Harriet.Db.Installation.Update_Installation
                       (Industry_Map (Next_Industry))
                         .Set_Resource (Resource)
                       .Done;
                  end loop;
                  Remaining_Industry := Remaining_Industry - Assigned_Industry;

               end;

            end if;

         end if;

      end Assign_Mines;

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

         Remaining_Strip_Mines := Manager.Available_Strip_Mines;
         Remaining_Mines := Manager.Available_Mines;
         Remaining_Industry := Manager.Available_Industry;

         Manager.Log
           ("found" & Manager.Available_Mines'Image & " mines and"
            & Manager.Available_Industry'Image & " industry");
      end Initialize_Installation_Maps;

   begin
      Initialize_Installation_Maps;
      Requirements.Scan_Commodity_Requirements
        (Manager.Resources, Assign_Mines'Access);
   end Process_Requirements;

end Harriet.Managers.Colonies;
