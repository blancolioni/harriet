with Harriet.Db.Facility;

package body Harriet.Installations is

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
