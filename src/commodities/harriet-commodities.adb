with Harriet.Db.Commodity;
with Harriet.Db.Input_Commodity;
with Harriet.Db.Manufactured;
with Harriet.Db.Stock_Item;

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

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (To        : in out Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
   is
      use type Harriet.Quantities.Quantity_Type;
      Tag : constant String := Harriet.Db.Commodity.Get (Commodity).Tag;
   begin
      if To.Contains (Tag) then
         declare
            Q : Harriet.Quantities.Quantity_Type renames To (Tag);
         begin
            Q := Q + Quantity;
         end;
      else
         To.Insert (Tag, Quantity);
      end if;
   end Add_Stock;

   ----------------------
   -- Current_Quantity --
   ----------------------

   function Current_Quantity
     (Of_Stock  : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
      Stock_Item : constant Harriet.Db.Stock_Item.Stock_Item_Type :=
        Harriet.Db.Stock_Item.Get_By_Stock_Item
          (Of_Stock, Commodity);
   begin
      if Stock_Item.Has_Element then
         return Stock_Item.Quantity;
      else
         return Harriet.Quantities.Zero;
      end if;
   end Current_Quantity;

   ----------------------
   -- Current_Quantity --
   ----------------------

   function Current_Quantity
     (Of_Stock  : Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
      Tag : constant String := Harriet.Db.Commodity.Get (Commodity).Tag;
   begin
      if Of_Stock.Contains (Tag) then
         return Of_Stock.Element (Tag);
      else
         return Harriet.Quantities.Zero;
      end if;
   end Current_Quantity;

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

   ---------------------
   -- Is_Manufactured --
   ---------------------

   function Is_Manufactured
     (Commodity : Commodity_Reference)
      return Boolean
   is
      use Harriet.Db;
   begin
      return Harriet.Db.Commodity.Get (Commodity)
        .Top_Record = R_Manufactured;
   end Is_Manufactured;

   -----------------
   -- Is_Resource --
   -----------------

   function Is_Resource
     (Commodity : Commodity_Reference)
      return Boolean
   is
      use Harriet.Db;
   begin
      return Harriet.Db.Commodity.Get (Commodity)
        .Top_Record = R_Resource;
   end Is_Resource;

   -------------------
   -- Raw_Resources --
   -------------------

   function Raw_Resources return Commodity_Reference is
   begin
      return Harriet.Db.Commodity.Get_Reference_By_Tag ("raw-resources");
   end Raw_Resources;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (From      : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
   is
      use Harriet.Quantities;
      Stock : constant Harriet.Db.Stock_Item.Stock_Item_Type :=
        Harriet.Db.Stock_Item.Get_By_Stock_Item (From, Commodity);
   begin
      pragma Assert (Stock.Has_Element or else Quantity = Zero);
      pragma Assert (Quantity = Zero or else Quantity <= Stock.Quantity);
      if Quantity > Zero then
         Harriet.Db.Stock_Item.Update_Stock_Item
           (Stock.Get_Stock_Item_Reference)
           .Set_Quantity (Stock.Quantity - Quantity)
           .Done;
      end if;
   end Remove_Stock;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Stock   : Stock_Type;
      Process : not null access
        procedure (Commodity : Harriet.Db.Commodity_Reference;
                   Quantity  : Harriet.Quantities.Quantity_Type))
   is
   begin
      for Position in Stock.Iterate loop
         Process
           (Harriet.Db.Commodity.Get_Reference_By_Tag
              (Stock_Maps.Key (Position)),
            Stock_Maps.Element (Position));
      end loop;
   end Scan;

   ----------------------
   -- Scan_Ingredients --
   ----------------------

   procedure Scan_Ingredients
     (Commodity : Commodity_Reference;
      Process   : not null access
        procedure (Ingredient : Commodity_Reference;
                   Quantity   : Harriet.Quantities.Quantity_Type))
   is
   begin
      for Input of
        Harriet.Db.Input_Commodity.Select_By_Manufactured
          (Harriet.Db.Manufactured.Get_Manufactured (Commodity)
           .Get_Manufactured_Reference)
      loop
         Process (Input.Commodity, Input.Quantity);
      end loop;
   end Scan_Ingredients;

end Harriet.Commodities;
