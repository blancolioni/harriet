with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db.Installation;

package Harriet.Installations is

   function Daily_Cost
     (Installation : Harriet.Db.Installation_Reference)
      return Harriet.Money.Money_Type;

   function Daily_Cost
     (Installation : Harriet.Db.Installation.Installation_Type)
      return Harriet.Money.Money_Type;

   function Required_Population
     (Installation : Harriet.Db.Installation_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Required_Population
     (Installation : Harriet.Db.Installation.Installation_Type)
      return Harriet.Quantities.Quantity_Type;

end Harriet.Installations;
