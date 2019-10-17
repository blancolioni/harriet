with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with WL.Random;

with Harriet.Commodities.Maps;
with Harriet.Pops;
with Harriet.Properties;
with Harriet.Utility;

with Harriet.Managers.Agents;

with Harriet.Money;
with Harriet.Quantities;
with Harriet.Random;
with Harriet.Real_Images;

with Harriet.Weighted_Random_Choices;

with Harriet.Db.Production;
with Harriet.Db.Input_Item;
with Harriet.Db.Output_Item;

with Harriet.Db.Market;
with Harriet.Db.Pop;
with Harriet.Db.Utility_Function;
with Harriet.Db.World_Sector;
with Harriet.Db.Zone;

package body Harriet.Managers.Pops is

   Annealing_Rounds : constant := 100;

   type Bid_Record is
      record
         Commodity : Harriet.Commodities.Commodity_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Price     : Harriet.Money.Price_Type;
      end record;

   package Bid_Vectors is
     new Ada.Containers.Vectors
       (Positive, Bid_Record);

   package Utility_Maps is
     new Harriet.Commodities.Maps
       (Harriet.Utility.Utility_Function, Harriet.Utility."=");

   type Root_Pop_Manager_Type is
     new Harriet.Managers.Agents.Root_Agent_Manager_Type with
      record
         Pop           : Harriet.Db.Pop_Reference;
         Production    : Harriet.Db.Production_Reference;
         Employer      : Harriet.Db.Employer_Reference;
         Utility_Class : Harriet.Db.Utility_Class_Reference;
         Size          : Harriet.Quantities.Quantity_Type;
         Utility_Fns   : Utility_Maps.Map;
         Sector        : Harriet.Db.World_Sector_Reference;
         World         : Harriet.Db.World_Reference;
         Current_Bids  : Bid_Vectors.Vector;
      end record;

   overriding procedure Create_Planning
     (Manager : in out Root_Pop_Manager_Type);

   overriding procedure Create_Bids
     (Manager : in out Root_Pop_Manager_Type);

   overriding procedure Execute_Consumption
     (Manager : in out Root_Pop_Manager_Type);

   overriding procedure Execute_Production
     (Manager : in out Root_Pop_Manager_Type);

   overriding function Identifier
     (Manager : Root_Pop_Manager_Type)
      return String;

   function Evaluate_Utility
     (Manager : Root_Pop_Manager_Type'Class;
      Bids    : Bid_Vectors.Vector)
      return Real;

   function Evaluate_Total_Cost
     (Manager : Root_Pop_Manager_Type'Class;
      Bids    : Bid_Vectors.Vector)
      return Harriet.Money.Money_Type;

   function Create_Utility_Function
     (Rec : Harriet.Db.Utility_Function.Utility_Function_Type)
      return Harriet.Utility.Utility_Function;

   procedure Choose_Production
     (Manager : in out Root_Pop_Manager_Type'Class);

   -----------------------
   -- Choose_Production --
   -----------------------

   procedure Choose_Production
     (Manager : in out Root_Pop_Manager_Type'Class)
   is

      type Production_Record is
         record
            Production : Harriet.Db.Production_Reference;
            Sector     : Harriet.Db.World_Sector_Reference;
            Sector_Use : Harriet.Db.Sector_Use_Reference;
            Zone       : Harriet.Commodities.Commodity_Reference;
            Skill      : Harriet.Quantities.Quantity_Type;
            Size       : Non_Negative_Real;
         end record;

      package Production_Choices is
        new Harriet.Weighted_Random_Choices (Production_Record);

      Skills : constant Harriet.Commodities.Commodity_Array :=
        Harriet.Commodities.Skills;

      Choice : Production_Choices.Weighted_Choice_Set;

      procedure Add_Choice
        (Production : Harriet.Db.Production_Reference;
         Skill      : Harriet.Commodities.Commodity_Reference);

      function Score_World_Sector_Production
        (World_Sector : Harriet.Db.World_Sector_Reference;
         Production   : Harriet.Db.Production_Reference;
         Sector_Use   : Harriet.Db.Sector_Use_Reference;
         Zone         : Harriet.Commodities.Commodity_Reference)
        return Natural;

      ----------------
      -- Add_Choice --
      ----------------

      procedure Add_Choice
        (Production : Harriet.Db.Production_Reference;
         Skill      : Harriet.Commodities.Commodity_Reference)
      is
         Zone           : constant Harriet.Db.Zone_Reference :=
           Harriet.Db.Production.Get (Production).Zone;
         Sector_Use     : constant Harriet.Db.Sector_Use_Reference :=
           Harriet.Db.Zone.Get (Zone).Sector_Use;
         Zone_Commodity : constant Harriet.Commodities.Commodity_Reference :=
           Harriet.Commodities.Get_Commodity (Zone);
      begin
         for World_Sector of
           Harriet.Db.World_Sector.Select_By_World_Sector_Use
             (Manager.World, Sector_Use)
         loop
            declare
               Score : constant Natural :=
                 Score_World_Sector_Production
                   (World_Sector.Get_World_Sector_Reference,
                    Production, Sector_Use, Zone_Commodity);
            begin
               if Score > 0 then
                  Choice.Insert
                    (Item  =>
                       Production_Record'
                         (Production => Production,
                          Sector     =>
                            World_Sector.Get_World_Sector_Reference,
                          Sector_Use =>
                            Sector_Use,
                          Zone       => Zone_Commodity,
                          Skill      =>
                            Manager.Stock_Quantity (Skill),
                          Size       =>
                            Harriet.Db.Production.Get (Production).Size),
                     Score => Score);
               end if;
            end;
         end loop;
      end Add_Choice;

      -----------------------------------
      -- Score_World_Sector_Production --
      -----------------------------------

      function Score_World_Sector_Production
        (World_Sector   : Harriet.Db.World_Sector_Reference;
         Production     : Harriet.Db.Production_Reference;
         Sector_Use     : Harriet.Db.Sector_Use_Reference;
         Zone           : Harriet.Commodities.Commodity_Reference)
         return Natural
      is
         pragma Unreferenced (Production, Sector_Use);
         use Harriet.Quantities;
         Title     : constant Harriet.Commodities.Commodity_Reference :=
           Harriet.Commodities.Title_Commodity
             (World_Sector, Zone);
         Leases    : constant Harriet.Commodities.Commodity_Array :=
           Commodities.Lease_Commodities (Title);
         Available : Quantity_Type := Zero;
      begin
         for Lease of Leases loop
            Available := Available + Manager.Current_Ask_Quantity (Lease);
         end loop;

         if Available > Manager.Size then
            return Natural (To_Real (Manager.Size) * 2.0);
         else
            return Natural (To_Real (Available) ** 2 / To_Real (Manager.Size));
         end if;
      end Score_World_Sector_Production;

   begin

      for Commodity of Skills loop
         if Manager.Has_Stock (Commodity) then
            for Input_Item of
              Harriet.Db.Input_Item.Select_By_Commodity
                (Harriet.Commodities.To_Database_Reference (Commodity))
            loop
               Add_Choice (Input_Item.Production, Commodity);
            end loop;
         end if;
      end loop;

      if Choice.Is_Empty then
         Manager.Log ("no available production");
      else
         declare
            use Harriet.Quantities;
            Production_Choice : constant Production_Record :=
              Choice.Choose;
            Production : constant Harriet.Db.Production_Reference :=
              Production_Choice.Production;
            Sector            : constant Harriet.Db.World_Sector_Reference :=
              Production_Choice.Sector;
            Title             : constant Commodities.Commodity_Reference :=
              Harriet.Commodities.Title_Commodity
                (Sector, Production_Choice.Zone);
            Leases            : constant Commodities.Commodity_Array :=
              Commodities.Lease_Commodities (Title);
            Quantity          : constant Quantity_Type :=
              Harriet.Quantities.Min
                (Manager.Current_Ask_Quantity (Title),
                 Scale (Manager.Size, Production_Choice.Size));
            Remaining         : Quantity_Type := Quantity;
         begin

            Harriet.Db.Pop.Update_Pop (Manager.Pop)
              .Set_World_Sector (Sector)
              .Set_Production (Production)
              .Done;

            for Lease of Leases loop
               declare
                  Lease_Quantity : constant Quantity_Type :=
                    Min (Remaining,
                         Manager.Current_Ask_Quantity (Lease));
               begin
                  if Lease_Quantity > Zero then
                     Manager.Create_Bid
                       (Commodity => Lease,
                        Quantity  => Lease_Quantity,
                        Price     =>
                          Manager.Current_Ask_Price (Lease, Quantity));
                     Remaining := Remaining - Quantity;
                  end if;
                  exit when Remaining = Zero;
               end;
            end loop;

            Manager.Production := Production;
            Manager.Sector := Sector;

            Manager.Log
              ("production: "
               & Harriet.Db.Production.Get (Production).Tag);
         end;
      end if;

   end Choose_Production;

   -----------------
   -- Create_Bids --
   -----------------

   overriding procedure Create_Bids
     (Manager : in out Root_Pop_Manager_Type)
   is
   begin
      for Item of Manager.Current_Bids loop
         declare
            use Harriet.Quantities;
            Commodity : constant Harriet.Commodities.Commodity_Reference :=
              Item.Commodity;
            Quantity  : constant Quantity_Type :=
              Item.Quantity;
            Have      : constant Quantity_Type :=
              Manager.Stock_Quantity (Commodity);
            Factor    : constant Real := 0.1;
            Want      : constant Quantity_Type :=
              Scale (Quantity, Factor);
            Bid       : constant Quantity_Type := Want - Have;
            Price     : constant Harriet.Money.Price_Type :=
              Manager.Current_Ask_Price (Commodity, Want - Have);
         begin
            if Want > Have then
               Manager.Create_Bid
                 (Commodity, Bid, Price);
            end if;
         end;
      end loop;
   end Create_Bids;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      Pop     : constant Harriet.Db.Pop.Pop_Type :=
        Harriet.Db.Pop.Get_Pop (Managed);
      Manager : Root_Pop_Manager_Type :=
        Root_Pop_Manager_Type'
          (Harriet.Managers.Agents.Root_Agent_Manager_Type with
           Pop           => Pop.Get_Pop_Reference,
           Production    => Pop.Production,
           Employer      => Pop.Employer,
           Utility_Class => Pop.Utility_Class,
           Size          => Pop.Size,
           Sector        => Pop.World_Sector,
           World         => Pop.World,
           Current_Bids  => <>,
           Utility_Fns   => <>);
   begin
      for Utility_Fn of
        Harriet.Db.Utility_Function.Select_By_Utility_Class
          (Pop.Utility_Class)
      loop
         Manager.Utility_Fns.Insert
           (Harriet.Commodities.Get_Commodity (Utility_Fn.Commodity),
            Create_Utility_Function (Utility_Fn));
      end loop;

      Manager.Initialize_Agent_Manager
        (Agent             => Pop,
         Market            =>
           Harriet.Db.Market.Get_Reference_By_World (Pop.World),
         Planning_Cycle    => 10);
      return new Root_Pop_Manager_Type'(Manager);
   end Create_Default_Manager;

   ---------------------
   -- Create_Planning --
   ---------------------

   overriding procedure Create_Planning
     (Manager : in out Root_Pop_Manager_Type)
   is

      use type Harriet.Money.Money_Type;

      Budget : constant Harriet.Money.Money_Type :=
        Harriet.Money.Max
          (Harriet.Money.Adjust (Manager.Current_Cash, 0.5),
           Harriet.Money.Adjust
             (Manager.Last_Earn - Manager.Last_Spend, 10.0));

      Work : Bid_Vectors.Vector renames Manager.Current_Bids;

      Prop_Hours : constant Harriet.Properties.Property_Type :=
                     Harriet.Properties.Property ("hours");
      Prop_Happy : constant Harriet.Properties.Property_Type :=
                     Harriet.Properties.Property ("happy");
      Prop_Health_1 : constant Harriet.Properties.Property_Type :=
                      Harriet.Properties.Property ("health-1");
      Prop_Health_2 : constant Harriet.Properties.Property_Type :=
        Harriet.Properties.Property ("health-2");
      Prop_Health_3 : constant Harriet.Properties.Property_Type :=
        Harriet.Properties.Property ("health-3");
      Prop_Ed       : constant Harriet.Properties.Property_Type :=
                  Harriet.Properties.Property ("education");

      function Current_Cost return Harriet.Money.Money_Type;

      procedure Initial_Value
        (Commodity : Harriet.Commodities.Commodity_Reference);

      function Current_Utility return Real;

      procedure Show_Work;

      procedure Log_Work;

      ------------------
      -- Current_Cost --
      ------------------

      function Current_Cost return Harriet.Money.Money_Type is
         use Harriet.Money;
      begin
         return Cost : Money_Type := Zero do
            for Item of Work loop
               Cost := Cost + Total (Item.Price, Item.Quantity);
            end loop;
         end return;
      end Current_Cost;

      ---------------------
      -- Current_Utility --
      ---------------------

      function Current_Utility return Real is
         use Harriet.Money;

         Penalty : Real := 0.0;
         Cost : constant Money_Type :=
                  Manager.Evaluate_Total_Cost (Work);

      begin

         if Cost > Budget then
            Penalty := To_Real (Cost) / To_Real (Budget) - 1.0;
         elsif Cost < Budget then
            Penalty :=
              1.0 - To_Real (Cost) / To_Real (Budget);
         end if;

         return Manager.Evaluate_Utility (Work) - Penalty;
      end Current_Utility;

      -------------------
      -- Initial_Value --
      -------------------

      procedure Initial_Value
        (Commodity : Harriet.Commodities.Commodity_Reference)
      is
         X : constant Non_Negative_Real := 1.0;
         Quantity : constant Harriet.Quantities.Quantity_Type :=
           Harriet.Quantities.To_Quantity (X);
         Item : constant Bid_Record :=
           Bid_Record'
             (Commodity => Commodity,
              Quantity  => Harriet.Quantities.To_Quantity (X),
              Price     => Manager.Current_Ask_Price (Commodity, Quantity));
      begin
         if Manager.Available (Commodity) then
            if Harriet.Commodities.Has_Property
              (Commodity, Prop_Hours)
              or else Harriet.Commodities.Has_Property
                (Commodity, Prop_Happy)
              or else Harriet.Commodities.Has_Property
                (Commodity, Prop_Health_1)
              or else Harriet.Commodities.Has_Property
                (Commodity, Prop_Health_2)
              or else Harriet.Commodities.Has_Property
                (Commodity, Prop_Health_3)
              or else Harriet.Commodities.Has_Property
                (Commodity, Prop_Ed)
            then
               Work.Append (Item);
            end if;
         end if;

         for Item of Work loop
            Item.Quantity :=
              Harriet.Quantities.Scale
                (Harriet.Money.Get_Quantity (Budget, Item.Price),
                 0.5 / Real (Work.Length));
         end loop;
      end Initial_Value;

      --------------
      -- Log_Work --
      --------------

      procedure Log_Work is
      begin
         for Item of Work loop
            Manager.Log
              ("want: "
               & Harriet.Quantities.Show (Item.Quantity)
               & " "
               & Harriet.Commodities.Local_Name (Item.Commodity));
         end loop;
         Manager.Log
           ("cost: "
            & Harriet.Money.Show
              (Manager.Evaluate_Total_Cost (Work))
            & "; utility "
            & Harriet.Real_Images.Approximate_Image
              (Manager.Evaluate_Utility (Work)));
      end Log_Work;

      ---------------
      -- Show_Work --
      ---------------

      procedure Show_Work is
         use Ada.Text_IO;

         procedure Put_Property
           (Commodity : Harriet.Commodities.Commodity_Reference;
            Quantity  : Harriet.Quantities.Quantity_Type;
            Name      : String);

         ------------------
         -- Put_Property --
         ------------------

         procedure Put_Property
           (Commodity : Harriet.Commodities.Commodity_Reference;
            Quantity  : Harriet.Quantities.Quantity_Type;
            Name      : String)
         is
            P : constant Harriet.Properties.Property_Type :=
              Harriet.Properties.Property (Name);
         begin
            if Harriet.Commodities.Has_Property (Commodity, P) then
               Put (Harriet.Quantities.Show
                    (Harriet.Quantities.Scale
                       (Quantity,
                          Harriet.Commodities.Get_Property
                            (Commodity, P))));
            else
               Put ("-");
            end if;
         end Put_Property;

      begin
         Put ("ITEM");
         Set_Col (20);
         Put ("Q");
         Set_Col (25);
         Put ("Price");
         Set_Col (35);
         Put ("Total");
         Set_Col (45);
         Put ("Hours");
         Set_Col (55);
         Put ("Happy");
         Set_Col (65);
         Put ("Health");
         Set_Col (75);
         Put ("Education");
         New_Line;

         for Item of Work loop
            Put (Harriet.Commodities.Local_Name (Item.Commodity));
            Set_Col (18);
            Put (Harriet.Quantities.Show (Item.Quantity));
            Set_Col (25);
            Put (Harriet.Money.Show (Item.Price));
            Set_Col (35);
            Put (Harriet.Money.Show
                 (Harriet.Money.Total (Item.Price, Item.Quantity)));
            Set_Col (45);
            Put_Property (Item.Commodity, Item.Quantity, "hours");
            Set_Col (55);
            Put_Property (Item.Commodity, Item.Quantity, "happy");
            Set_Col (65);
            Put_Property (Item.Commodity, Item.Quantity, "health");
            Set_Col (75);
            Put_Property (Item.Commodity, Item.Quantity, "education");
            New_Line;
         end loop;

         Ada.Text_IO.Put_Line
           ("Utility: "
            & Harriet.Real_Images.Approximate_Image
              (Manager.Evaluate_Utility (Work)));
         Ada.Text_IO.Put_Line
           ("Cost: "
            & Harriet.Money.Show
              (Manager.Evaluate_Total_Cost (Work)));

      end Show_Work;

      Now : constant Ada.Calendar.Time :=
        Ada.Calendar.Clock;

   begin

      Manager.Log ("planning budget: " & Harriet.Money.Show (Budget));

      if Budget <= Harriet.Money.Zero then
         return;
      end if;

      Work.Clear;

      for Commodity of Harriet.Commodities.All_Commodities loop
         Initial_Value (Commodity);
      end loop;

      if Work.Is_Empty then
         Manager.Log ("nothing available");
         return;
      end if;

      for I in 1 .. Annealing_Rounds loop
         declare
            use Harriet.Money, Harriet.Quantities;
            Old_Utility : constant Real := Current_Utility;
            Old_Work    : constant Bid_Vectors.Vector := Work;
            Change_Index : constant Positive :=
              WL.Random.Random_Number (1, Work.Last_Index);
            Commodity    : constant Harriet.Commodities.Commodity_Reference :=
              Work.Element (Change_Index).Commodity;
            Quantity     : constant Quantity_Type :=
              Work.Element (Change_Index).Quantity;
--              Price        : constant Price_Type :=
--                Commodity.Base_Price;
            Old_Cost     : constant Money_Type := Current_Cost;
--              Max          : constant Quantity_Type :=
--                (if Old_Cost < Budget
--                 then Get_Quantity (Budget - Old_Cost, Price)
--                 else Zero);
            Target_Value : constant Quantity_Type :=
              (if WL.Random.Random_Number (1, 2) = 1
               then Scale (Quantity, 0.75)
               else Scale (Quantity, 1.25));
         begin
            Work (Change_Index).Quantity := Target_Value;
            if (Current_Utility < Old_Utility
                or else (Current_Cost > Old_Cost
                         and then Current_Cost > Budget))
              and then Harriet.Random.Unit_Random
                < Real (I) / Real (Annealing_Rounds)
            then
               Work := Old_Work;
            elsif False then
               Ada.Text_IO.Put (Harriet.Commodities.Local_Name (Commodity));
               Ada.Text_IO.Set_Col (16);
               Ada.Text_IO.Put (Harriet.Quantities.Show (Quantity));
               Ada.Text_IO.Set_Col (26);
               Ada.Text_IO.Put (Harriet.Quantities.Show (Target_Value));
               Ada.Text_IO.Set_Col (36);
               Ada.Text_IO.Put (Harriet.Money.Show (Current_Cost));
               Ada.Text_IO.Set_Col (46);
               Ada.Text_IO.Put
                 (Harriet.Real_Images.Approximate_Image
                    (Current_Utility));
               Ada.Text_IO.New_Line;
            end if;
         end;
      end loop;

      Log_Work;

      if False then
         Show_Work;
      end if;

      declare
         use Ada.Calendar;
         Interval : constant Duration :=
           Clock - Now;
      begin
         Manager.Log ("planning complete in "
                      & Real_Images.Approximate_Image
                        (Real (Interval) * 1000.0) & "ms");
      end;

   end Create_Planning;

   -----------------------------
   -- Create_Utility_Function --
   -----------------------------

   function Create_Utility_Function
     (Rec : Harriet.Db.Utility_Function.Utility_Function_Type)
      return Harriet.Utility.Utility_Function
   is
   begin
      if Rec.Tag = "polynomial" then
         return Harriet.Utility.Quadratic
           (Rec.A1, Rec.A2, Rec.A3, Rec.A4, Rec.A5, Rec.A6);
      elsif Rec.Tag = "logarithmic" then
         return Harriet.Utility.Logarithmic
           (Rec.A1, Rec.A2);
      elsif Rec.Tag = "zero" then
         return Harriet.Utility.Zero;
      else
         raise Constraint_Error with
           "no such utility function: " & Rec.Tag;
      end if;
   end Create_Utility_Function;

   -------------------------
   -- Evaluate_Total_Cost --
   -------------------------

   function Evaluate_Total_Cost
     (Manager : Root_Pop_Manager_Type'Class;
      Bids    : Bid_Vectors.Vector)
      return Harriet.Money.Money_Type
   is
      pragma Unreferenced (Manager);
      use Harriet.Money;
   begin
      return Total_Cost : Money_Type := Zero do
         for Item of Bids loop
            Total_Cost := Total_Cost + Total (Item.Price, Item.Quantity);
         end loop;
      end return;
   end Evaluate_Total_Cost;

   ----------------------
   -- Evaluate_Utility --
   ----------------------

   function Evaluate_Utility
     (Manager : Root_Pop_Manager_Type'Class;
      Bids    : Bid_Vectors.Vector)
      return Real
   is
      Size : constant Non_Negative_Real :=
        Harriet.Quantities.To_Real (Manager.Size);

      Hours      : Real := 0.0;
      Happy      : Real := 0.0;
      Health     : Real := 0.0;
      Health_Set : array (1 .. 3) of Real :=
        (others => 0.0);
      Education : Real := 0.0;

      Available : Harriet.Quantities.Quantity_Type :=
        Harriet.Quantities.Zero;

      Missing   : Harriet.Quantities.Quantity_Type :=
        Harriet.Quantities.Zero;

      procedure Apply
        (Commodity : Harriet.Commodities.Commodity_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Value     : Harriet.Money.Money_Type);

      function Evaluate
        (Name : String;
         X    : Real)
         return Real;

      -----------
      -- Apply --
      -----------

      procedure Apply
        (Commodity : Harriet.Commodities.Commodity_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Value     : Harriet.Money.Money_Type)
      is
         pragma Unreferenced (Value);

         use type Harriet.Quantities.Quantity_Type;

         Have : constant Harriet.Quantities.Quantity_Type :=
           Manager.Stock_Quantity (Commodity);

         procedure Update
           (Acc       : in out Real;
            Name      : String;
            Cost_Name : String := "");

         ------------
         -- Update --
         ------------

         procedure Update
           (Acc       : in out Real;
            Name      : String;
            Cost_Name : String := "")
         is
            use Harriet.Commodities;
            P : constant Harriet.Properties.Property_Type :=
              Harriet.Properties.Property (Name);
         begin
            if Has_Property (Commodity, P) then
               Acc := Acc +
                 Harriet.Quantities.To_Real (Quantity)
                 * Get_Property (Commodity, P)
                 + Harriet.Quantities.To_Real (Available)
                 * Get_Property (Commodity, P);
            end if;

            if Cost_Name /= "" then
               declare
                  C : constant Harriet.Properties.Property_Type :=
                    Harriet.Properties.Property (Cost_Name);
               begin
                  if Has_Property (Commodity, C) then
                     Acc := Acc - Get_Property (Commodity, C);
                  end if;
               end;
            end if;

         end Update;

      begin
         Update (Hours, "hours", "preparation");
         Update (Happy, "happy");
         Update (Health_Set (1), "health-1");
         Update (Health_Set (2), "health-1");
         Update (Health_Set (3), "health-1");
         Update (Education, "education");

         Missing := Missing +
           Quantity - Harriet.Quantities.Min (Have, Quantity);

         Available := Available + Harriet.Quantities.Min (Have, Quantity);

      end Apply;

      --------------
      -- Evaluate --
      --------------

      function Evaluate
        (Name : String;
         X    : Real)
         return Real
      is
         use Harriet.Commodities;
         Commodity : constant Commodity_Reference :=
                       Get (Name);
         Fn        : constant Harriet.Utility.Utility_Function :=
                       Manager.Utility_Fns.Element (Commodity);
      begin
         return Fn.Evaluate (X / Size) + 1.0;
      end Evaluate;

   begin

      for Item of Bids loop
         Apply (Item.Commodity, Item.Quantity,
                Harriet.Money.Total (Item.Price, Item.Quantity));
      end loop;

      Health := Real'Last;
      for H of Health_Set loop
         Health := Real'Min (Health, H);
      end loop;

      declare
         use type Harriet.Quantities.Quantity_Type;
         Hour_Utility : constant Real :=
           Evaluate ("hours", Hours);
         Happy_Utility : constant Real :=
           Evaluate ("happy", Happy);
         Health_Utility : constant Real :=
           Evaluate ("health", Health);
         Education_Utility : constant Real :=
           Evaluate ("education", Education);
         Base_Utility : constant Real :=
           Hour_Utility
           + Happy_Utility
           + Health_Utility
           + Education_Utility;
         Availability_Factor : constant Real :=
           0.5
           + Harriet.Quantities.To_Real (Available)
           / Harriet.Quantities.To_Real (Available + Missing);
         Final_Utility : constant Real :=
           (if Base_Utility < 0.0 then Base_Utility
            else Base_Utility * Availability_Factor);
      begin
         return Final_Utility;
      end;

   end Evaluate_Utility;

   -------------------------
   -- Execute_Consumption --
   -------------------------

   overriding procedure Execute_Consumption
     (Manager : in out Root_Pop_Manager_Type)
   is
      use Harriet.Quantities;

      Size : constant Non_Negative_Real :=
        To_Real (Manager.Size);

      Hours, Happy : Non_Negative_Real := 0.0;
      Health_Set   : array (1 .. 3) of Non_Negative_Real :=
        (others => 0.0);
      Health       : Non_Negative_Real := 0.0;

   begin
      for Item of Manager.Current_Bids loop
         declare
            Commodity : constant Harriet.Commodities.Commodity_Reference :=
              Item.Commodity;
            Quantity  : constant Quantity_Type :=
              Item.Quantity;
            Have      : constant Quantity_Type :=
              Manager.Stock_Quantity (Commodity);
            Factor    : constant Real := 0.1;
            Consume   : constant Quantity_Type :=
              Min (Scale (Quantity, Factor), Have);

            procedure Record_Consumption
              (Property_Name : String;
               Target        : in out Non_Negative_Real);

            ------------------------
            -- Record_Consumption --
            ------------------------

            procedure Record_Consumption
              (Property_Name : String;
               Target        : in out Non_Negative_Real)
            is
               Provided : constant Non_Negative_Real :=
                 Harriet.Commodities.Get_Property
                   (Commodity, Harriet.Properties.Property (Property_Name));
               Total    : constant Non_Negative_Real :=
                 Provided * To_Real (Consume);
            begin
               Target := Target + Total / Size;
            end Record_Consumption;

         begin
            Manager.Log
              ("consume "
               & Show (Consume)
               & " "
               & Harriet.Commodities.Local_Name
                 (Commodity));

            Record_Consumption ("hours", Hours);
            Record_Consumption ("happy", Happy);
            Record_Consumption ("health-1", Health_Set (1));
            Record_Consumption ("health-2", Health_Set (2));
            Record_Consumption ("health-3", Health_Set (3));
            Manager.Remove_Stock (Commodity, Consume);
         end;
      end loop;

      declare
         Current_Happy : constant Non_Negative_Real :=
           Harriet.Pops.Happy (Manager.Pop);
         Current_Health : constant Non_Negative_Real :=
           Harriet.Pops.Health (Manager.Pop);
         Current_Hours  : constant Non_Negative_Real :=
           Harriet.Pops.Hours (Manager.Pop);
      begin
         Health := Real'Last;
         for H of Health_Set loop
            Health := Real'Min (Health, H);
         end loop;

         Harriet.Db.Pop.Update_Pop (Manager.Pop)
           .Set_Happy (Current_Happy * 0.9 + Happy)
           .Set_Health (Current_Health * 0.95 + Health * 0.5)
           .Set_Hours (Current_Hours * 0.5 + Hours * 5.0)
           .Done;
         Manager.Log
           ("metric: happy: old="
            & Harriet.Real_Images.Approximate_Image (Current_Happy)
            & " consumption="
            & Harriet.Real_Images.Approximate_Image (Happy)
            & " new="
            & Harriet.Real_Images.Approximate_Image
              (Harriet.Pops.Happy (Manager.Pop)));
         Manager.Log
           ("metric: health: old="
            & Harriet.Real_Images.Approximate_Image (Current_Health)
            & " consumption="
            & Harriet.Real_Images.Approximate_Image (Health)
            & " new="
            & Harriet.Real_Images.Approximate_Image
              (Harriet.Pops.Health (Manager.Pop)));
         Manager.Log
           ("metric: hours: old="
            & Harriet.Real_Images.Approximate_Image (Current_Hours)
            & " consumption="
            & Harriet.Real_Images.Approximate_Image (Hours)
            & " new="
            & Harriet.Real_Images.Approximate_Image
              (Harriet.Pops.Hours (Manager.Pop)));
      end;

   end Execute_Consumption;

   ------------------------
   -- Execute_Production --
   ------------------------

   overriding procedure Execute_Production
     (Manager : in out Root_Pop_Manager_Type)
   is
      Max_Capacity    : Non_Negative_Real := 0.0;

      Production_Cost : Harriet.Money.Money_Type :=
                          Harriet.Money.Zero;

      type Bid_Record is
         record
            Commodity  : Harriet.Commodities.Commodity_Reference;
            Quantity   : Harriet.Quantities.Quantity_Type;
            Price      : Harriet.Money.Price_Type;
            Total_Cost : Harriet.Money.Money_Type;
            Capacity   : Non_Negative_Real;
         end record;

      package Bid_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Bid_Record);

      Bids : Bid_Lists.List;

      function Constrain
        (Commodity : Harriet.Commodities.Commodity_Reference;
         Required  : Harriet.Quantities.Quantity_Type)
         return Non_Negative_Real;

      function Evaluate_Cost
        (Commodity : Harriet.Commodities.Commodity_Reference;
         Used      : Harriet.Quantities.Quantity_Type)
         return Harriet.Money.Money_Type;

      function Zone_Capacity return Non_Negative_Real;
      function Zone_Cost return Harriet.Money.Money_Type;

      ---------------
      -- Constrain --
      ---------------

      function Constrain
        (Commodity : Harriet.Commodities.Commodity_Reference;
         Required  : Harriet.Quantities.Quantity_Type)
         return Non_Negative_Real
      is
         Available : constant Harriet.Quantities.Quantity_Type :=
           Manager.Stock_Quantity (Commodity);
      begin
         return Harriet.Quantities.To_Real (Available)
           / Harriet.Quantities.To_Real (Required);
      end Constrain;

      -------------------
      -- Evaluate_Cost --
      -------------------

      function Evaluate_Cost
        (Commodity : Harriet.Commodities.Commodity_Reference;
         Used      : Harriet.Quantities.Quantity_Type)
         return Harriet.Money.Money_Type
      is
         use Harriet.Money, Harriet.Quantities;
         Price    : constant Price_Type :=
           Manager.Current_Ask_Price (Commodity, Used);
         Leased   : constant Harriet.Commodities.Commodity_Array :=
           Harriet.Commodities.Lease_Commodities (Commodity);
         Cost     : Money_Type := Zero;
         Quantity : Quantity_Type := Used;
      begin
         for Lease of Leased loop
            declare
               Lease_Quantity : constant Quantity_Type :=
                 Min (Manager.Stock_Quantity (Lease), Quantity);
               Lease_Cost     : constant Money_Type :=
                 Manager.Stock_Value (Lease);
            begin
               Cost := Cost + Lease_Cost;
               Quantity := Quantity - Lease_Quantity;
            end;
         end loop;

         return Cost + Harriet.Money.Total (Price, Quantity);

      end Evaluate_Cost;

      -------------------
      -- Zone_Capacity --
      -------------------

      function Zone_Capacity return Non_Negative_Real is
         Production : constant Harriet.Db.Production.Production_Type :=
           Harriet.Db.Production.Get (Manager.Production);
         Title      : constant Harriet.Commodities.Commodity_Reference :=
           Harriet.Commodities.Title_Commodity
             (Manager.Sector, Commodities.Get_Commodity (Production.Zone));
         Size : constant Non_Negative_Real := Production.Size;
         Have : constant Non_Negative_Real :=
           Harriet.Quantities.To_Real
             (Manager.Stock_Quantity (Title));
      begin
         return Have / Size;
      end Zone_Capacity;

      ---------------
      -- Zone_Cost --
      ---------------

      function Zone_Cost return Harriet.Money.Money_Type is
         use Harriet.Money, Harriet.Quantities;
         Production : constant Harriet.Db.Production.Production_Type :=
           Harriet.Db.Production.Get (Manager.Production);
         Zone       : constant Harriet.Commodities.Commodity_Reference :=
           Harriet.Commodities.Get_Commodity (Production.Zone);
         Title      : constant Harriet.Commodities.Commodity_Reference :=
           Harriet.Commodities.Title_Commodity
             (Manager.Sector, Zone);
         Have       : Harriet.Quantities.Quantity_Type :=
           Manager.Stock_Quantity (Title);
         Leases     : constant Harriet.Commodities.Commodity_Array :=
           Harriet.Commodities.Lease_Commodities (Title);
         Cost       : Money_Type := Zero;
      begin
         for Lease of Leases loop
            Cost := Cost + Manager.Stock_Value (Lease);
            Have := Have - Manager.Stock_Quantity (Lease);
         end loop;

         Cost := Cost + Total (Manager.Stock_Price (Title), Have);
         return Cost;
      end Zone_Cost;

      Budget : constant Harriet.Money.Money_Type :=
        Harriet.Money.Adjust (Manager.Current_Cash, 0.5);

   begin

      declare
         use Harriet.Db;
      begin
         if Manager.Production = Null_Production_Reference
           and then Manager.Employer = Null_Employer_Reference
         then
            Manager.Choose_Production;
         end if;

         if Manager.Production = Null_Production_Reference then
            return;
         end if;
      end;

      declare
         use Harriet.Money;
      begin
         if Budget <= Zero then
            return;
         end if;
      end;

      Max_Capacity := Zone_Capacity;

      for Input_Item of
        Harriet.Db.Input_Item.Select_By_Production (Manager.Production)
      loop
         if Harriet.Commodities.Is_Skill
           (Harriet.Commodities.Get_Commodity (Input_Item.Commodity))
         then
            declare
               This_Capacity : constant Non_Negative_Real :=
                 Constrain
                   (Harriet.Commodities.Get_Commodity
                      (Input_Item.Commodity),
                    Input_Item.Quantity);
            begin
               Max_Capacity := Real'Min (This_Capacity, Max_Capacity);
            end;
         end if;
      end loop;

      if Max_Capacity = 0.0 then
         Manager.Production := Harriet.Db.Null_Production_Reference;
         Harriet.Db.Pop.Update_Pop (Manager.Pop)
           .Set_Production (Manager.Production)
           .Done;
         return;
      end if;

      Manager.Log ("skill capacity: "
                   & Real_Images.Approximate_Image (Max_Capacity));

      declare
         use Harriet.Quantities, Harriet.Money;
         Total_Quantity : Quantity_Type := Zero;
         Total_Cost     : Money_Type    := Zero;
      begin
         for Input_Item of
           Harriet.Db.Input_Item.Select_By_Production (Manager.Production)
         loop
            declare
               This_Capacity : constant Non_Negative_Real :=
                 Constrain
                   (Harriet.Commodities.Get_Commodity (Input_Item.Commodity),
                    Input_Item.Quantity);
            begin
               if This_Capacity < Max_Capacity then
                  declare

                     Commodity : constant Commodities.Commodity_Reference :=
                       Commodities.Get_Commodity
                         (Input_Item.Commodity);

                     Quantity : constant Quantity_Type :=
                       Scale (Input_Item.Quantity,
                              Max_Capacity - This_Capacity);

                     Price    : constant Price_Type :=
                       Manager.Current_Ask_Price (Commodity, Quantity);

                  begin
                     Bids.Append
                       (Bid_Record'
                          (Commodity => Commodity,
                           Quantity  => Quantity,
                           Price     => Price,
                           Total_Cost => Total (Price, Quantity),
                           Capacity   => Max_Capacity - This_Capacity));
                     Total_Quantity := Total_Quantity + Quantity;
                     Total_Cost := Total_Cost + Total (Price, Quantity);
                  end;
               end if;
            end;
         end loop;

         if Total_Cost > Budget then
            declare

               Remaining : Money_Type := Total_Cost - Budget;
               Total_Score : Non_Negative_Real := 0.0;

               function Score (Bid : Bid_Record) return Real
               is (if Bid.Total_Cost = Zero then Real'Last
                   else Bid.Capacity / To_Real (Bid.Total_Cost));

               function Provides_Less
                 (Left, Right : Bid_Record)
                  return Boolean
               is (Score (Left) < Score (Right));

               package Return_Sorting is
                 new Bid_Lists.Generic_Sorting (Provides_Less);

            begin
               Return_Sorting.Sort (Bids);

               for Bid of Bids loop
                  if Bid.Total_Cost > Zero then
                     Manager.Log
                       (Show (Bid.Quantity)
                        & " "
                        & Commodities.Local_Name (Bid.Commodity)
                        & " @ "
                        & Show (Bid.Price)
                        & "; total "
                        & Show (Bid.Total_Cost)
                        & "; capacity "
                        & Real_Images.Approximate_Image (Bid.Capacity)
                        & " score "
                        & Real_Images.Approximate_Image (Score (Bid)));

                     Total_Score := Total_Score + Score (Bid);
                  end if;
               end loop;

               for Bid of Bids loop
                  if Bid.Total_Cost > Zero then
                     declare
                        Contribution : constant Money_Type :=
                          Adjust (Bid.Total_Cost, Score (Bid) / Total_Score);
                        Final        : constant Money_Type :=
                          Min (Contribution, Remaining);
                     begin
                        Bid.Capacity := Bid.Capacity
                          * To_Real (Bid.Total_Cost - Final)
                          / To_Real (Bid.Total_Cost);
                        Bid.Quantity := Get_Quantity (Final, Bid.Price);
                        Bid.Total_Cost := Bid.Total_Cost - Final;
                        Remaining := Remaining - Final;
                        exit when Remaining = Zero;
                     end;
                  end if;
               end loop;
            end;
         end if;
      end;

      for Bid of Bids loop
         Manager.Create_Bid
           (Commodity => Bid.Commodity,
            Quantity  => Bid.Quantity,
            Price     => Bid.Price);
      end loop;

      for Input_Item of
        Harriet.Db.Input_Item.Select_By_Production (Manager.Production)
      loop
         declare
            This_Capacity : constant Non_Negative_Real :=
              Constrain
                (Harriet.Commodities.Get_Commodity (Input_Item.Commodity),
                 Input_Item.Quantity);
         begin
            Max_Capacity := Real'Min (This_Capacity, Max_Capacity);
         end;
      end loop;

      Manager.Log ("capacity: "
                   & Harriet.Real_Images.Approximate_Image
                     (Max_Capacity));

      Production_Cost := Zone_Cost;

      Manager.Log
        ("production cost: location " & Harriet.Money.Show (Production_Cost));

      for Input_Item of
        Harriet.Db.Input_Item.Select_By_Production (Manager.Production)
      loop
         declare
            use Harriet.Commodities;
            Commodity : constant Commodity_Reference :=
              Get_Commodity (Input_Item.Commodity);
         begin
            if not Is_Skill (Commodity) then
               declare
                  use Harriet.Money;
                  This_Quantity : constant Harriet.Quantities.Quantity_Type :=
                    Harriet.Quantities.Scale
                      (Input_Item.Quantity, Max_Capacity);
                  This_Cost     : constant Money_Type :=
                    Evaluate_Cost (Commodity, This_Quantity);
               begin
                  Manager.Log
                    (Harriet.Commodities.Local_Name (Commodity)
                     & " cost " & Show (This_Cost));
                  Production_Cost := Production_Cost + This_Cost;
               end;
            end if;
         end;
      end loop;

      declare
         Factor : constant Real :=
                    Harriet.Random.Normal_Random (0.1);
         Output : constant Non_Negative_Real :=
                    Real'Max (0.0, (1.0 + Factor) * Max_Capacity);
      begin
         for Out_Item of
           Harriet.Db.Output_Item.Select_By_Production
             (Manager.Production)
         loop
            declare
               Commodity : constant Harriet.Commodities.Commodity_Reference :=
                 Harriet.Commodities.Get_Commodity (Out_Item.Commodity);
               Quantity : constant Harriet.Quantities.Quantity_Type :=
                 Harriet.Quantities.Scale
                   (Out_Item.Quantity, Output);
               Minimum_Price : constant Harriet.Money.Price_Type :=
                 Harriet.Money.Price
                   (Production_Cost, Quantity);
            begin
               Manager.Log
                 ("produce " & Harriet.Quantities.Show (Quantity)
                  & " "
                  & Harriet.Commodities.Local_Name (Commodity)
                  & " for "
                  & Harriet.Money.Show (Minimum_Price)
                  & " ea");
               Manager.Add_Stock
                 (Commodity => Commodity,
                  Quantity  => Quantity,
                  Value     =>
                    Production_Cost);

            end;

            declare
               use Harriet.Money, Harriet.Quantities;
               Commodity : constant Harriet.Commodities.Commodity_Reference :=
                 Harriet.Commodities.Get_Commodity (Out_Item.Commodity);
               Quantity  : constant Harriet.Quantities.Quantity_Type :=
                 Manager.Stock_Quantity
                   (Commodity);
               Previous      : constant Quantity_Type :=
                 Manager.Previous_Ask (Commodity);
               Remaining     : constant Quantity_Type :=
                 Manager.Remaining_Ask (Commodity);
               Ask           : Quantity_Type := Quantity;
               Minimum_Price : constant Harriet.Money.Price_Type :=
                 Manager.Stock_Price (Commodity);
               Mean_Price     : constant Price_Type :=
                 Manager.Historical_Mean_Price
                   (Commodity);
               Base_Ask_Price : constant Price_Type := Mean_Price;
               Discount_Price : constant Price_Type :=
                 Adjust_Price (Base_Ask_Price, 0.99);
               Previous_Price : constant Price_Type :=
                 Manager.Previous_Ask_Price
                   (Commodity);
               Supply         : constant Quantity_Type :=
                 Manager.Historical_Supply
                   (Commodity, Harriet.Calendar.Days (1));
               Supply_At_Ask  : constant Quantity_Type :=
                 Manager.Historical_Supply
                   (Commodity, Previous_Price,
                    Harriet.Calendar.Days (1));
               Demand         : constant Quantity_Type :=
                 Manager.Historical_Demand
                   (Commodity, Harriet.Calendar.Days (1));
               Demand_At_Ask  : constant Quantity_Type :=
                 Manager.Historical_Demand
                   (Commodity, Previous_Price,
                    Harriet.Calendar.Days (1));
               Ask_Price      : constant Price_Type :=
                 (if Previous_Price > Zero
                  and then Previous_Price <= Base_Ask_Price
                  and then Remaining < Scale (Previous, 0.1)
                  then Previous_Price
                  elsif Discount_Price > Minimum_Price
                  and then Remaining > Zero
                  then Discount_Price
                  elsif Mean_Price > Minimum_Price
                  then Mean_Price
                  else Adjust_Price (Minimum_Price, 1.1));
            begin

               Manager.Log
                 ("price: base=" & Show (Base_Ask_Price)
                  & "; mean=" & Show (Mean_Price)
                  & "; discount=" & Show (Discount_Price)
                  & "; previous=" & Show (Previous_Price)
                  & "; minimum=" & Show (Minimum_Price)
                  & "; ask=" & Show (Ask_Price));
               Manager.Log
                 ("market: supply=" & Show (Supply)
                  & " at ask=" & Show (Supply_At_Ask)
                  & "; demand=" & Show (Demand)
                  & " at ask=" & Show (Demand_At_Ask)
                  & "; offered=" & Show (Previous)
                  & "; sold=" & Show (Previous - Remaining)
                  & "; remaining=" & Show (Remaining));

               if Previous > Zero then
                  if Remaining = Zero then
                     Ask := Scale (Previous, 1.1);
                  elsif Remaining < Scale (Previous, 0.9) then
                     Ask := Scale (Previous, 0.8);
                  else
                     Ask := Previous;
                  end if;
               end if;

               Manager.Log
                 (Commodities.Local_Name (Commodity)
                  & " previous ask "
                  & Show (Previous) & " @ " & Show (Previous_Price)
                  & "; sold " & Show (Previous - Remaining)
                  & "; remaining " & Show (Remaining)
                  & "; new ask "
                  & Show (Ask) & " @ " & Show (Ask_Price));

               Manager.Create_Ask
                 (Commodity => Commodity,
                  Quantity  => Min (Ask, Quantity),
                  Price     => Ask_Price);
            end;
         end loop;

      end;

   end Execute_Production;

   ----------------
   -- Identifier --
   ----------------

   overriding function Identifier
     (Manager : Root_Pop_Manager_Type)
      return String
   is
   begin
      return "pop" & Harriet.Db.To_String (Manager.Pop) & " manager";
   end Identifier;

end Harriet.Managers.Pops;
