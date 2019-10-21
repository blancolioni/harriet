with Harriet.Money;
with Harriet.Quantities;
with Harriet.Real_Images;

with Harriet.Colonies;
with Harriet.Factions;
with Harriet.Worlds;

with Harriet.Db.Colony;
with Harriet.Db.Expense;
with Harriet.Db.Revenue;

package body Harriet.Managers.Colonies is

   type Root_Colony_Manager is new Root_Manager_Type with
      record
         Colony      : Harriet.Db.Colony_Reference;
         Faction     : Harriet.Db.Faction_Reference;
         World       : Harriet.Db.World_Reference;
         Total_Pop   : Harriet.Quantities.Quantity_Type;
         Working_Pop : Harriet.Quantities.Quantity_Type;
         Idle_Pop    : Harriet.Quantities.Quantity_Type;
      end record;

   overriding function Identifier
     (Manager : Root_Colony_Manager)
      return String
   is ("colony" & Harriet.Db.To_String (Manager.Colony) & " manager");

   overriding procedure Activate
     (Manager : not null access Root_Colony_Manager);

   procedure Check_Revenue
     (Manager : Root_Colony_Manager'Class);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Colony_Manager)
   is
      Colony : constant Harriet.Colonies.Colony_Handle :=
        Harriet.Colonies.Get (Manager.Colony);
   begin
      Manager.Log
        (Harriet.Factions.Name (Harriet.Factions.Get (Manager.Faction))
         & " colony on "
         & Harriet.Worlds.Name (Manager.World)
         & " activating");

      Manager.Total_Pop := Colony.Population;

      Manager.Check_Revenue;

      Manager.Set_Next_Update_Delay (Harriet.Calendar.Days (1));
   end Activate;

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
                     Idle_Pop        => Harriet.Quantities.Zero);
   begin
      Manager.Colony := Colony.Get_Colony_Reference;
      return new Root_Colony_Manager'(Manager);
   end Create_Default_Manager;

end Harriet.Managers.Colonies;
