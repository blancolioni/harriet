with Harriet.Quantities;

with Harriet.Db;

package Harriet.Commodities is

   subtype Commodity_Reference is Harriet.Db.Commodity_Reference;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return Commodity_Reference
     with Pre => Exists (Tag);

   function Raw_Resources return Commodity_Reference;

   procedure Add_Stock
     (To        : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type);

end Harriet.Commodities;
