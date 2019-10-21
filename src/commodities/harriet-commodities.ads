private with WL.String_Maps;

with Harriet.Quantities;

with Harriet.Db;

package Harriet.Commodities is

   subtype Commodity_Reference is Harriet.Db.Commodity_Reference;

   type Array_Of_Commodities is
     array (Positive range <>) of Harriet.Db.Commodity_Reference;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return Commodity_Reference
     with Pre => Exists (Tag);

   function Is_Resource
     (Commodity : Commodity_Reference)
      return Boolean;

   function Is_Manufactured
     (Commodity : Commodity_Reference)
      return Boolean;

   procedure Scan_Ingredients
     (Commodity : Commodity_Reference;
      Process   : not null access
        procedure (Ingredient : Commodity_Reference;
                   Quantity   : Harriet.Quantities.Quantity_Type));

   function Raw_Resources return Commodity_Reference;

   function Current_Quantity
     (Of_Stock  : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   procedure Add_Stock
     (To        : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type);

   procedure Remove_Stock
     (From      : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type);

   type Stock_Type is private;

   function Current_Quantity
     (Of_Stock  : Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   procedure Add_Stock
     (To        : in out Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type);

   procedure Scan
     (Stock : Stock_Type;
      Process : not null access
        procedure (Commodity : Harriet.Db.Commodity_Reference;
                   Quantity  : Harriet.Quantities.Quantity_Type));

private

   package Stock_Maps is
     new WL.String_Maps (Harriet.Quantities.Quantity_Type,
                         Harriet.Quantities."=");

   type Stock_Type is new Stock_Maps.Map with null record;

end Harriet.Commodities;
