with Ada.Containers.Vectors;

with Harriet.Calendar;
with Harriet.Elementary_Functions;
with Harriet.Money;
with Harriet.Quantities;
with Harriet.Real_Images;

with Harriet.Colonies;
with Harriet.Installations;
with Harriet.Factions;
with Harriet.Worlds;

with Harriet.Db.Colony;
with Harriet.Db.Facility;
with Harriet.Db.Installation;
with Harriet.Db.Faction;
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

   function Population_Update
     (Colony  : Harriet.Db.Colony.Colony_Type;
      World   : Harriet.Db.World.World_Type;
      Faction : Harriet.Db.Faction.Faction_Type)
      return Harriet.Quantities.Quantity_Type;

   function Tax_Revenue
     (Colony  : Harriet.Db.Colony.Colony_Type;
      World   : Harriet.Db.World.World_Type;
      Faction : Harriet.Db.Faction.Faction_Type)
      return Harriet.Money.Money_Type;

   procedure Update_Installations
     (Colony  : Harriet.Colonies.Colony_Handle;
      World   : Harriet.Db.World_Reference;
      Faction : Harriet.Factions.Faction_Type'Class;
      Pop     : Harriet.Quantities.Quantity_Type;
      Cash    : in out Harriet.Money.Money_Type);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Colony_Update)
   is
      use type Harriet.Money.Money_Type;
      Colony  : constant Harriet.Db.Colony.Colony_Type :=
                  Harriet.Db.Colony.Get (Update.Colony);
      World   : constant Harriet.Db.World.World_Type :=
                  Harriet.Db.World.Get (Colony.World);
      Faction : constant Harriet.Db.Faction.Faction_Type :=
                  Harriet.Db.Faction.Get (Colony.Faction);
      New_Pop : constant Harriet.Quantities.Quantity_Type :=
                  Population_Update (Colony, World, Faction);
      Revenue : constant Harriet.Money.Money_Type :=
                  Tax_Revenue (Colony, World, Faction);
      Cash    : Harriet.Money.Money_Type :=
                  Faction.Cash + Revenue;
   begin

      Update_Installations
        (Colony  => Get (Colony.Get_Colony_Reference),
         World   => Colony.World,
         Faction => Harriet.Factions.Get (Colony.Faction),
         Pop     => New_Pop,
         Cash    => Cash);

      Harriet.Db.Colony.Update_Colony (Update.Colony)
        .Set_Population (New_Pop)
        .Set_Wealth (Colony.Wealth - Revenue)
        .Done;

      Harriet.Db.Faction.Update_Faction (Colony.Faction)
        .Set_Cash (Cash)
        .Done;

      Harriet.Factions.Synchronise (Colony.Faction);

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

   -----------------------
   -- Population_Update --
   -----------------------

   function Population_Update
     (Colony  : Harriet.Db.Colony.Colony_Type;
      World   : Harriet.Db.World.World_Type;
      Faction : Harriet.Db.Faction.Faction_Type)
      return Harriet.Quantities.Quantity_Type
   is
      use Harriet.Real_Images;
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
      return Harriet.Quantities.To_Quantity (New_Pop);
   end Population_Update;

   -----------------
   -- Tax_Revenue --
   -----------------

   function Tax_Revenue
     (Colony  : Harriet.Db.Colony.Colony_Type;
      World   : Harriet.Db.World.World_Type;
      Faction : Harriet.Db.Faction.Faction_Type)
      return Harriet.Money.Money_Type
   is
      use Harriet.Elementary_Functions;
      use Harriet.Money;
      use Harriet.Real_Images;
      Current_Wealth : constant Money_Type := Colony.Wealth;
      Tax_Rate       : constant Unit_Real := Colony.Tax_Rate;
      Tax_Evasion    : constant Unit_Real := Colony.Tax_Evasion;
      Loyalty        : constant Unit_Real := Colony.Loyalty;
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
      return Revenue;

   end Tax_Revenue;

   -----------------------
   -- Update_Installations --
   -----------------------

   procedure Update_Installations
     (Colony  : Harriet.Colonies.Colony_Handle;
      World   : Harriet.Db.World_Reference;
      Faction : Harriet.Factions.Faction_Type'Class;
      Pop     : Harriet.Quantities.Quantity_Type;
      Cash    : in out Harriet.Money.Money_Type)
   is
      pragma Unreferenced (Faction, Colony);
      use Harriet.Money, Harriet.Quantities;

      type Installation_Record is
         record
            Installation : Harriet.Db.Installation_Reference;
            Cost         : Harriet.Money.Money_Type;
            Employees    : Harriet.Quantities.Quantity_Type;
            Power        : Natural;
         end record;

      package Installation_Vectors is
        new Ada.Containers.Vectors (Positive, Installation_Record);

      Installations   : Installation_Vectors.Vector;
      Efficiency      : Unit_Real := 1.0;
      Total_Cost      : Money_Type := Zero;
      Total_Pop       : Quantity_Type := Zero;
      Total_Power     : Natural := 0;
      Available_Power : Natural := 0;

   begin
      for Installation of Harriet.Db.Installation.Select_By_World (World) loop

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
            Power     : constant Natural :=
                          (if Facility.Generator
                           then 0
                           else Facility.Power);
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
            Efficiency := Efficiency * 0.99;
            if Facility.Generator then
               Available_Power := Available_Power + Facility.Power;
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

         Cash := New_Cash;
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

      for Rec of Installations loop
         Harriet.Db.Installation.Update_Installation (Rec.Installation)
           .Set_Efficiency (Efficiency)
           .Done;
      end loop;

   end Update_Installations;

end Harriet.Colonies.Updates;
