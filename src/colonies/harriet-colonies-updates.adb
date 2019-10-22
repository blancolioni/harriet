with Ada.Containers.Vectors;

with Harriet.Calendar;
with Harriet.Elementary_Functions;
with Harriet.Money;
with Harriet.Quantities;
with Harriet.Real_Images;

with Harriet.Colonies;
with Harriet.Commodities;
with Harriet.Installations;
with Harriet.Factions;
with Harriet.Worlds;

with Harriet.Db.Colony;
with Harriet.Db.Expense;
with Harriet.Db.Facility;
with Harriet.Db.Facility_Input;
with Harriet.Db.Faction;
with Harriet.Db.Installation;
with Harriet.Db.Revenue;
with Harriet.Db.World;

with Harriet.Logging;

with Harriet.Updates.Events;

package body Harriet.Colonies.Updates is

   type Colony_Update is
     new Harriet.Updates.Update_Interface with
      record
         Colony : Harriet.Db.Colony_Reference;
      end record;

   overriding procedure Activate
     (Update : Colony_Update);

   procedure Population_Update
     (Update  : Colony_Update'Class);

   procedure Tax_Revenue
     (Update  : Colony_Update'Class);

   procedure Installations_Update
     (Update : Colony_Update'Class);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Colony_Update)
   is
   begin

      Update.Population_Update;
      Update.Installations_Update;
      Update.Tax_Revenue;

      Harriet.Factions.Synchronise
        (Harriet.Db.Colony.Get (Update.Colony).Faction);

      Harriet.Updates.Events.Update_With_Delay
        (Harriet.Calendar.Days (1), Update);
   end Activate;

   ------------------
   -- Daily_Update --
   ------------------

   function Daily_Update
     (Reference : Harriet.Db.Colony_Reference)
     return Harriet.Updates.Update_Interface'Class
   is
   begin
      return Colony_Update'(Colony => Reference);
   end Daily_Update;

   --------------------------
   -- Installations_Update --
   --------------------------

   procedure Installations_Update
     (Update : Colony_Update'Class)
   is
      use Harriet.Money, Harriet.Quantities;
      Colony  : constant Harriet.Db.Colony.Colony_Type :=
        Harriet.Db.Colony.Get (Update.Colony);
      Faction           : constant Harriet.Db.Faction.Faction_Type :=
        Harriet.Db.Faction.Get (Colony.Faction);
      World             : constant Harriet.Db.World_Reference :=
        Colony.World;
      Stock             : constant Harriet.Db.Has_Stock_Reference :=
        Colony.Get_Has_Stock_Reference;
      Pop               : constant Harriet.Quantities.Quantity_Type :=
        Colony.Population;
      Cash              : constant Harriet.Money.Money_Type :=
        Faction.Cash;
      Free_Installations : Natural := 10;

      type Installation_Record is
         record
            Installation : Harriet.Db.Installation_Reference;
            Cost         : Harriet.Money.Money_Type;
            Employees    : Harriet.Quantities.Quantity_Type;
            Power        : Natural;
         end record;

      package Installation_Vectors is
        new Ada.Containers.Vectors (Positive, Installation_Record);

      type Generator_Record is
         record
            Installation : Harriet.Db.Installation_Reference;
            Max_Power    : Natural;
            Consumption  : Harriet.Commodities.Stock_Type;
         end record;

      package Generator_Vectors is
        new Ada.Containers.Vectors (Positive, Generator_Record);

      Installations   : Installation_Vectors.Vector;
      Generators      : Generator_Vectors.Vector;
      Efficiency      : Unit_Real := Colony.Loyalty;
      Total_Cost      : Money_Type := Zero;
      Total_Pop       : Quantity_Type := Zero;
      Total_Power     : Natural := 0;
      Available_Power : Natural := 0;
      Generated_Power : Natural := 0;
      Total_Count     : Natural := 0;

   begin

      for Installation of Harriet.Db.Installation.Select_By_World (World) loop
         Free_Installations := Free_Installations
           + Harriet.Db.Facility.Get (Installation.Facility)
           .Free_Installations;
      end loop;

      for Installation of Harriet.Db.Installation.Select_By_World (World) loop

         Total_Count := Total_Count + 1;

         declare
            Reference : constant Harriet.Db.Installation_Reference :=
              Installation.Get_Installation_Reference;
            Cost      : constant Money_Type :=
              Harriet.Installations.Daily_Cost (Installation);
            Employees : constant Quantity_Type :=
              Harriet.Installations.Required_Population
                (Installation);
            Facility  : constant Harriet.Db.Facility.Facility_Type :=
              Harriet.Db.Facility.Get
                (Installation.Facility);
            Power     : constant Natural := Facility.Power;
         begin
            Installations.Append
              (Installation_Record'
                 (Installation => Reference,
                  Cost         => Cost,
                  Employees    => Employees,
                  Power        => Power));
            Total_Cost := Total_Cost + Cost;
            Total_Pop := Total_Pop + Employees;
            Total_Power := Total_Power + Power;

            if Total_Count > Free_Installations then
               Efficiency := Efficiency * 0.95;
            end if;

            if Facility.Generate > 0 then
               declare
                  Max : Natural := Facility.Generate;
               begin
                  for Input of
                    Harriet.Db.Facility_Input.Select_By_Facility
                      (Installation.Facility)
                  loop
                     declare
                        Required      : constant Quantity_Type :=
                          Input.Quantity;
                        Available     : constant Quantity_Type :=
                          Harriet.Commodities.Current_Quantity
                            (Stock, Input.Commodity);
                        Consumed      : constant Quantity_Type :=
                          Min (Required, Available);
                        Effectiveness : constant Unit_Real :=
                          To_Real (Consumed) / To_Real (Required);
                        New_Max       : constant Natural :=
                          Natural (Real (Facility.Generate) * Effectiveness);
                     begin
                        Max := Natural'Min (Max, New_Max);
                     end;
                  end loop;

                  if Max > 0 then
                     Available_Power := Available_Power + Max;
                     declare
                        Rec : Generator_Record :=
                          Generator_Record'
                            (Installation =>
                               Installation.Get_Installation_Reference,
                             Max_Power    => Max,
                             Consumption  => <>);
                     begin
                        for Input of
                          Harriet.Db.Facility_Input.Select_By_Facility
                            (Installation.Facility)
                        loop
                           declare
                              Required      : constant Quantity_Type :=
                                Input.Quantity;
                              Available     : constant Quantity_Type :=
                                Harriet.Commodities.Current_Quantity
                                  (Stock, Input.Commodity);
                              Consumed      : constant Quantity_Type :=
                                Scale (Required,
                                       Real (Max) / Real (Facility.Generate));
                           begin
                              Harriet.Commodities.Remove_Stock
                                (Stock, Input.Commodity,
                                 Min (Available, Consumed));
                              Harriet.Commodities.Add_Stock
                                (Rec.Consumption, Input.Commodity,
                                 Min (Available, Consumed));
                           end;
                        end loop;
                        Generators.Append (Rec);
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;

      if Pop < Total_Pop then
         Efficiency :=
           Real'Min (Efficiency, To_Real (Pop) / To_Real (Total_Pop));
      end if;

      Harriet.Logging.Log
        (Harriet.Worlds.Name (World),
         "required pop: " & Show (Total_Pop)
         & "; available pop: " & Show (Pop)
         & "; efficiency:"
         & Natural'Image (Natural (Efficiency * 100.0)) & "%");

      Harriet.Db.Expense.Create
        (Colony  => Update.Colony,
         Date    => Harriet.Calendar.Clock,
         Expense => Total_Cost);

      declare
         New_Cash : Money_Type;
      begin
         if Cash < Total_Cost then
            Efficiency :=
              Real'Min (Efficiency, To_Real (Cash) / To_Real (Total_Cost));
            New_Cash := Zero;
         else
            New_Cash := Cash - Total_Cost;
         end if;

         Harriet.Logging.Log
           (Harriet.Worlds.Name (World),
            "total cost: " & Show (Total_Cost)
            & "; cash: " & Show (Cash)
            & "; remaining cash: " & Show (New_Cash)
            & "; efficiency:"
            & Natural'Image (Natural (Efficiency * 100.0)) & "%");

         Harriet.Db.Faction.Update_Faction (Colony.Faction)
           .Set_Cash (New_Cash)
           .Done;

      end;

      if Total_Power > Available_Power then
         Efficiency :=
           Real'Min (Efficiency, Real (Available_Power) / Real (Total_Power));
      end if;

      Harriet.Logging.Log
        (Harriet.Worlds.Name (World),
         "total power:" & Total_Power'Image
         & "; available power:" & Available_Power'Image
         & "; efficiency:"
         & Natural'Image (Natural (Efficiency * 100.0)) & "%");

      Generated_Power := Natural'Min (Total_Power, Available_Power);

      for Generator of Generators loop
         exit when Generated_Power = 0;
         declare
            Scale          : constant Unit_Real :=
              Real (Generated_Power)
              / Real (Available_Power);

            procedure Consume_Input
              (Commodity : Harriet.Commodities.Commodity_Reference;
               Quantity  : Quantity_Type);

            -------------------
            -- Consume_Input --
            -------------------

            procedure Consume_Input
              (Commodity : Harriet.Commodities.Commodity_Reference;
               Quantity  : Quantity_Type)
            is
               Consume : constant Quantity_Type :=
                 Min (Harriet.Commodities.Current_Quantity
                      (Generator.Consumption, Commodity),
                      Harriet.Quantities.Scale (Quantity, Scale));
            begin
               Harriet.Commodities.Add_Stock
                 (Stock, Commodity,
                  Harriet.Commodities.Current_Quantity
                    (Generator.Consumption, Commodity)
                  - Consume);

            end Consume_Input;

         begin
            Harriet.Commodities.Scan
              (Generator.Consumption, Consume_Input'Access);
         end;

      end loop;

      for Rec of Installations loop
         Harriet.Db.Installation.Update_Installation (Rec.Installation)
           .Set_Efficiency (Efficiency)
           .Done;
      end loop;

   end Installations_Update;

   -----------------------
   -- Population_Update --
   -----------------------

   procedure Population_Update
     (Update  : Colony_Update'Class)
   is
      use Harriet.Real_Images;
      Colony  : constant Harriet.Db.Colony.Colony_Type :=
        Harriet.Db.Colony.Get (Update.Colony);
      World   : constant Harriet.Db.World.World_Type :=
        Harriet.Db.World.Get (Colony.World);
      Faction : constant Harriet.Db.Faction.Faction_Type :=
        Harriet.Db.Faction.Get (Colony.Faction);
      Birth_Rate  : constant Unit_Real :=
                      0.02 * World.Habitability;
      Death_Rate  : constant Unit_Real :=
                      (2.0 - World.Habitability) / 200.0;
      Daily_Rate  : constant Unit_Real :=
                      (Birth_Rate - Death_Rate) / 365.0;
      Current_Pop : constant Non_Negative_Real :=
                      Harriet.Quantities.To_Real (Colony.Population);
      New_Pop     : constant Non_Negative_Real :=
                      Current_Pop * (1.0 + Daily_Rate);
   begin
      Harriet.Logging.Log
        (World.Name
         & " owned by "
         & Faction.Name,
         "habitability: "
         & Approximate_Image (World.Habitability * 100.0) & "%"
         & "; current pop "
         & Approximate_Image (Current_Pop)
         & "; birth rate: "
         & Approximate_Image (Birth_Rate * 100.0) & "%"
         & "; death rate: "
         & Approximate_Image (Death_Rate * 100.0) & "%"
         & "; change: "
         & Approximate_Image (New_Pop - Current_Pop)
         & "; new pop: "
         & Approximate_Image (New_Pop));

      Harriet.Db.Colony.Update_Colony (Update.Colony)
        .Set_Population (Harriet.Quantities.To_Quantity (New_Pop))
        .Done;

   end Population_Update;

   -----------------
   -- Tax_Revenue --
   -----------------

   procedure Tax_Revenue
     (Update  : Colony_Update'Class)
   is
      use Harriet.Elementary_Functions;
      use Harriet.Money;
      use Harriet.Real_Images;

      Colony  : constant Harriet.Db.Colony.Colony_Type :=
        Harriet.Db.Colony.Get (Update.Colony);
      World   : constant Harriet.Db.World.World_Type :=
        Harriet.Db.World.Get (Colony.World);
      Faction : constant Harriet.Db.Faction.Faction_Type :=
        Harriet.Db.Faction.Get (Colony.Faction);

      Current_Wealth : constant Money_Type := Colony.Wealth;
      Tax_Rate       : constant Unit_Real := Colony.Tax_Rate;
      Tax_Evasion    : constant Unit_Real := Colony.Tax_Evasion;
      New_Tax_Evasion : constant Unit_Real :=
        0.9 * Colony.Tax_Evasion + 0.1 * Colony.Tax_Rate ** 2;
      Loyalty         : constant Unit_Real := Colony.Loyalty;
      New_Loyalty     : constant Unit_Real :=
        0.9 * Colony.Loyalty + 0.1 * Sqrt (1.0 - Colony.Tax_Rate);
      Daily_Rate     : constant Unit_Real :=
                         (Tax_Rate * (1.0 - Tax_Evasion) * Sqrt (Loyalty))
                         / 365.0;
      Revenue        : constant Money_Type :=
                         Adjust (Current_Wealth, Daily_Rate);
   begin
      Harriet.Logging.Log
        (World.Name
         & " owned by "
         & Faction.Name,
         "wealth: " & Show (Current_Wealth)
         & "; tax rate: "
         & Approximate_Image (Tax_Rate * 100.0) & "%"
         & "; tax evasion: "
         & Approximate_Image (Tax_Evasion * 100.0) & "%"
         & "; loyalty: "
         & Approximate_Image (Loyalty * 100.0) & "%"
         & "; daily rate: "
         & Approximate_Image (Tax_Rate * 100.0) & "%"
         & "; revenue: "
         & Show (Revenue));

      Harriet.Db.Colony.Update_Colony (Update.Colony)
        .Set_Wealth (Colony.Wealth - Revenue)
        .Set_Tax_Evasion (New_Tax_Evasion)
        .Set_Loyalty (New_Loyalty)
        .Done;

      Harriet.Db.Faction.Update_Faction (Colony.Faction)
        .Set_Cash (Faction.Cash + Revenue)
        .Done;

      Harriet.Db.Revenue.Create
        (Colony  => Update.Colony,
         Date    => Harriet.Calendar.Clock,
         Revenue => Revenue);

   end Tax_Revenue;

end Harriet.Colonies.Updates;
