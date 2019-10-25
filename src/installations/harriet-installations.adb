with Harriet.Db.Colony;
with Harriet.Db.Facility;
with Harriet.Db.Production_Goal;

package body Harriet.Installations is

   procedure Add_Production_Goal
     (Colony    : Harriet.Db.Colony_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Priority  : Positive)
   is
      use type Harriet.Db.Commodity_Reference;
      use type Harriet.Quantities.Quantity_Type;
   begin
      for Rec of
        Harriet.Db.Production_Goal.Select_By_Colony (Colony)
      loop
         if Rec.Commodity = Commodity
           and then Rec.Priority = Priority
         then
            Harriet.Db.Production_Goal.Update_Production_Goal
              (Rec.Get_Production_Goal_Reference)
              .Set_Quantity (Rec.Quantity + Quantity)
              .Done;
            return;
         end if;
      end loop;

      Harriet.Db.Production_Goal.Create
        (Status    => Harriet.Db.Waiting,
         Faction   => Harriet.Db.Colony.Get (Colony).Faction,
         Priority  => Priority,
         Colony    => Colony,
         Commodity => Commodity,
         Quantity  => Quantity);

   end Add_Production_Goal;

   ----------------
   -- Daily_Cost --
   ----------------

   function Daily_Cost
     (Installation : Harriet.Db.Installation_Reference)
      return Harriet.Money.Money_Type
   is
   begin
      return Daily_Cost (Harriet.Db.Installation.Get (Installation));
   end Daily_Cost;

   ----------------
   -- Daily_Cost --
   ----------------

   function Daily_Cost
     (Installation : Harriet.Db.Installation.Installation_Type)
      return Harriet.Money.Money_Type
   is
   begin
      return Harriet.Db.Facility.Get (Installation.Facility).Operating_Cost;
   end Daily_Cost;

   -------------------------
   -- Required_Population --
   -------------------------

   function Required_Population
     (Installation : Harriet.Db.Installation_Reference)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Required_Population
        (Harriet.Db.Installation.Get (Installation));
   end Required_Population;

   -------------------------
   -- Required_Population --
   -------------------------

   function Required_Population
     (Installation : Harriet.Db.Installation.Installation_Type)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Db.Facility.Get (Installation.Facility)
        .Employees;
   end Required_Population;

end Harriet.Installations;
