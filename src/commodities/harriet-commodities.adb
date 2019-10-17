with Harriet.Db.Stock_Item;
with Harriet.Db.Commodity;

package body Harriet.Commodities is

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (To        : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
   is
      use Harriet.Db;
      use Harriet.Quantities;
      Item : constant Stock_Item.Stock_Item_Type :=
               Harriet.Db.Stock_Item.Get_By_Stock_Item
                 (To, Commodity);
   begin
      if Item.Has_Element then
         Harriet.Db.Stock_Item.Update_Stock_Item
           (Item.Get_Stock_Item_Reference)
           .Set_Quantity (Item.Quantity + Quantity)
           .Done;
      else
         Harriet.Db.Stock_Item.Create
           (To, Commodity, Quantity);
      end if;
   end Add_Stock;

   ------------
   -- Exists --
   ------------

   function Exists (Tag : String) return Boolean is
      use type Harriet.Db.Commodity_Reference;
   begin
      return Harriet.Db.Commodity.Get_Reference_By_Tag (Tag)
        /= Harriet.Db.Null_Commodity_Reference;
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return Commodity_Reference is
   begin
      return Harriet.Db.Commodity.Get_Reference_By_Tag (Tag);
   end Get;

   -------------------
   -- Raw_Resources --
   -------------------

   function Raw_Resources return Commodity_Reference is
   begin
      return Harriet.Db.Commodity.Get_Reference_By_Tag ("raw-resource");
   end Raw_Resources;

end Harriet.Commodities;
