with Harriet.Managers.Agents;

with Harriet.Commodities;
with Harriet.Stock;

with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db.Army;
with Harriet.Db.Market;
with Harriet.Db.Pop;
with Harriet.Db.Regiment;
with Harriet.Db.Supply_Input;
with Harriet.Db.Unit;

package body Harriet.Managers.Armies is

   type Root_Army_Manager is
     new Harriet.Managers.Agents.Root_Agent_Manager_Type with
      record
         Daily    : Harriet.Commodities.Stock_Type;
         Supplied : Harriet.Db.Supplied_Reference;
         Army     : Harriet.Db.Army_Reference;
      end record;

   overriding function Identifier
     (Manager : Root_Army_Manager)
      return String
   is ("army" & Harriet.Db.To_String (Manager.Army) & " manager");

   overriding procedure Create_Planning
     (Manager : in out Root_Army_Manager);

   overriding procedure Create_Bids
     (Manager : in out Root_Army_Manager);

   overriding procedure Execute_Consumption
     (Manager : in out Root_Army_Manager);

   overriding procedure Execute_Production
     (Manager : in out Root_Army_Manager)
   is null;

   overriding procedure Create_Bids
     (Manager : in out Root_Army_Manager)
   is

      procedure Bid
        (Commodity : Harriet.Commodities.Commodity_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Value     : Harriet.Money.Money_Type);

      ---------
      -- Bid --
      ---------

      procedure Bid
        (Commodity : Harriet.Commodities.Commodity_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Value     : Harriet.Money.Money_Type)
      is
         pragma Unreferenced (Value);
         use Harriet.Quantities;
         Want : constant Quantity_Type := Scale (Quantity, 10.0);
         Have : constant Quantity_Type := Manager.Stock_Quantity (Commodity);
         Max  : constant Quantity_Type := Scale (Quantity, 1.1);
         Bid_Quantity : constant Quantity_Type :=
           Min (Max, Want - Have);
      begin
         if Have < Want then
            Manager.Create_Bid
              (Commodity => Commodity,
               Quantity  => Bid_Quantity,
               Price     =>
                 Manager.Current_Ask_Price (Commodity, Bid_Quantity));
         end if;
      end Bid;

   begin
      Manager.Daily.Iterate (Bid'Access);
   end Create_Bids;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Harriet.Db.Managed_Reference) return Manager_Type
   is
      Manager : Root_Army_Manager;
      Army : constant Harriet.Db.Army.Army_Type :=
        Harriet.Db.Army.Get_Army (Managed);
   begin
      Manager.Army := Army.Get_Army_Reference;
      Manager.Supplied := Army.Get_Supplied_Reference;
      Manager.Initialize_Agent_Manager
        (Agent          => Army,
         Market         =>
           Harriet.Db.Market.Get_Reference_By_World
             (Army.World),
         Planning_Cycle => 10);
      return new Root_Army_Manager'(Manager);
   end Create_Default_Manager;

   ---------------------
   -- Create_Planning --
   ---------------------

   overriding procedure Create_Planning
     (Manager : in out Root_Army_Manager)
   is
      Daily : Harriet.Commodities.Stock_Type renames Manager.Daily;
   begin
      Daily.Clear;

      for Regiment of
        Harriet.Db.Regiment.Select_By_Army (Manager.Army)
      loop
         declare
            Unit : constant Harriet.Db.Unit.Unit_Type :=
              Harriet.Db.Unit.Get (Regiment.Unit);

            procedure Add_Supply
              (Supplied : Harriet.Db.Supplied_Reference);

            ----------------
            -- Add_Supply --
            ----------------

            procedure Add_Supply
              (Supplied : Harriet.Db.Supplied_Reference)
            is
            begin
               for Supply_Input of
                 Harriet.Db.Supply_Input.Select_By_Supplied (Supplied)
               loop
                  declare
                     Commodity : constant Commodities.Commodity_Reference :=
                       Commodities.Get_Commodity (Supply_Input.Commodity);
                  begin
                     Daily.Add_Quantity
                       (Commodity,
                        Supply_Input.Quantity,
                        Manager.Current_Ask_Price
                          (Commodity, Supply_Input.Quantity));
                  end;
               end loop;
            end Add_Supply;

         begin
            Add_Supply (Unit.Get_Supplied_Reference);
            Add_Supply (Regiment.Get_Supplied_Reference);
         end;

      end loop;

      Manager.Log
        ("daily budget: "
         & Harriet.Money.Show (Daily.Total_Value));

   end Create_Planning;

   -------------------------
   -- Execute_Consumption --
   -------------------------

   overriding procedure Execute_Consumption
     (Manager : in out Root_Army_Manager)
   is
      Daily : Harriet.Commodities.Stock_Type;
      Food  : Harriet.Commodities.Stock_Type;
      Count : Natural := 0;
   begin
      for Regiment of
        Harriet.Db.Regiment.Select_By_Army (Manager.Army)
      loop
         declare
            Unit : constant Harriet.Db.Unit.Unit_Type :=
              Harriet.Db.Unit.Get (Regiment.Unit);

            procedure Add
              (Stock : in out Harriet.Commodities.Stock_Type;
               Supplied : Harriet.Db.Supplied_Reference);

            ---------
            -- Add --
            ---------

            procedure Add
              (Stock    : in out Harriet.Commodities.Stock_Type;
               Supplied : Harriet.Db.Supplied_Reference)
            is
            begin
               for Supply_Input of
                 Harriet.Db.Supply_Input.Select_By_Supplied (Supplied)
               loop
                  declare
                     Commodity : constant Commodities.Commodity_Reference :=
                       Commodities.Get_Commodity (Supply_Input.Commodity);
                  begin
                     Stock.Add_Quantity
                       (Commodity,
                        Supply_Input.Quantity,
                        Manager.Current_Ask_Price
                          (Commodity, Supply_Input.Quantity));
                  end;
               end loop;
            end Add;

         begin
            Add (Daily, Unit.Get_Supplied_Reference);
            Add (Food, Regiment.Get_Supplied_Reference);
         end;

         Count := Count + 1;

      end loop;

      declare

         Organisation : Unit_Real := 1.0;

         procedure Consume
           (Commodity : Harriet.Commodities.Commodity_Reference;
            Quantity  : Harriet.Quantities.Quantity_Type;
            Value     : Harriet.Money.Money_Type);

         procedure Transfer
           (Commodity : Harriet.Commodities.Commodity_Reference;
            Quantity  : Harriet.Quantities.Quantity_Type;
            Value     : Harriet.Money.Money_Type);

         -------------
         -- Consume --
         -------------

         procedure Consume
           (Commodity : Harriet.Commodities.Commodity_Reference;
            Quantity  : Harriet.Quantities.Quantity_Type;
            Value     : Harriet.Money.Money_Type)
         is
            pragma Unreferenced (Value);
            use Harriet.Quantities;
            Have : constant Quantity_Type :=
              Manager.Stock_Quantity (Commodity);
            Consumed : constant Quantity_Type := Min (Have, Quantity);
         begin
            if Consumed < Quantity then
               Organisation :=
                 Unit_Real'Min (Organisation,
                                To_Real (Consumed)
                                / To_Real (Quantity));
            end if;
            Manager.Remove_Stock (Commodity, Consumed);
         end Consume;

         --------------
         -- Transfer --
         --------------

         procedure Transfer
           (Commodity : Harriet.Commodities.Commodity_Reference;
            Quantity  : Harriet.Quantities.Quantity_Type;
            Value     : Harriet.Money.Money_Type)
         is
            use Harriet.Quantities;
            Have     : constant Quantity_Type :=
              Manager.Stock_Quantity (Commodity);
            Consume  : constant Quantity_Type := Min (Have, Quantity);
            Factor   : constant Real :=
              To_Real (Consume) / To_Real (Quantity);
            Price    : constant Harriet.Money.Price_Type :=
              Harriet.Money.Price (Value, Quantity);
            Ref      : constant Harriet.Db.Commodity_Reference :=
              Harriet.Commodities.To_Database_Reference
                (Commodity);
         begin
            Manager.Remove_Stock (Commodity, Consume);

            for Regiment of
              Harriet.Db.Regiment.Select_By_Army (Manager.Army)
            loop
               declare
                  Input              : constant Harriet.Db.Supply_Input
                    .Supply_Input_Type :=
                      Harriet.Db.Supply_Input.Get_By_Supply_Input
                        (Regiment.Get_Supplied_Reference, Ref);
                  Want               : constant Quantity_Type :=
                    (if Input.Has_Element then Input.Quantity else Zero);
                  Transfered : constant Quantity_Type :=
                    Scale (Want, Factor);
               begin
                  if Transfered > Zero then
                     Harriet.Stock.Add_Stock
                       (To       =>
                          Harriet.Db.Pop.Get (Regiment.Pop)
                        .Get_Has_Stock_Reference,
                        Item     => Commodity,
                        Quantity => Transfered,
                        Value    =>
                          Harriet.Money.Total (Price, Transfered));
                  end if;
               end;
            end loop;
         end Transfer;

      begin
         Daily.Iterate (Consume'Access);
         Food.Iterate (Transfer'Access);

         for Regiment of
           Harriet.Db.Regiment.Select_By_Army (Manager.Army)
         loop
            Harriet.Db.Regiment.Update_Regiment
              (Regiment.Get_Regiment_Reference)
              .Set_Morale
                (Unit_Clamp
                   (Regiment.Morale
                    + (Organisation - Regiment.Morale) / 10.0))
                .Done;
         end loop;
      end;

   end Execute_Consumption;

end Harriet.Managers.Armies;
