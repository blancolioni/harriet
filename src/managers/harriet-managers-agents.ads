with Harriet.Commodities;
with Harriet.Markets;

with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db.Agent;

package Harriet.Managers.Agents is

   type Root_Agent_Manager_Type is
     abstract new Root_Manager_Type with private;

   overriding procedure Activate
     (Manager : not null access Root_Agent_Manager_Type);

   procedure Create_Planning
     (Manager : in out Root_Agent_Manager_Type)
   is abstract;

   procedure Create_Bids
     (Manager : in out Root_Agent_Manager_Type)
   is abstract;

   procedure Execute_Production
     (Manager : in out Root_Agent_Manager_Type)
   is abstract;

   procedure Execute_Consumption
     (Manager : in out Root_Agent_Manager_Type)
   is abstract;

   procedure On_Activation_Begin
     (Manager : in out Root_Agent_Manager_Type);

   procedure On_Activation_End
     (Manager : in out Root_Agent_Manager_Type);

   function Current_Cash
     (Manager : Root_Agent_Manager_Type'Class)
      return Harriet.Money.Money_Type;

   procedure Earn
     (Manager : Root_Agent_Manager_Type'Class;
      Amount  : Harriet.Money.Money_Type;
      Tag     : String);

   procedure Spend
     (Manager : Root_Agent_Manager_Type'Class;
      Amount  : Harriet.Money.Money_Type;
      Tag     : String);

   function Last_Earn
     (Manager : Root_Agent_Manager_Type'Class)
      return Harriet.Money.Money_Type;

   function Last_Spend
     (Manager : Root_Agent_Manager_Type'Class)
      return Harriet.Money.Money_Type;

   procedure Reset_Cashflow
     (Manager : Root_Agent_Manager_Type'Class);

   function Current_Ask_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
      return Harriet.Money.Price_Type;

   function Current_Bid_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
      return Harriet.Money.Price_Type;

   function Historical_Mean_Price
     (Manager : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Money.Price_Type;

   function Current_Buy_Cost
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
      return Harriet.Money.Money_Type;

   function Current_Sell_Earn
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
      return Harriet.Money.Money_Type;

   function Available
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Boolean;

   function Current_Ask_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Current_Bid_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Current_Ask_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Bid_Total : Harriet.Money.Money_Type)
      return Harriet.Quantities.Quantity_Type;

   function Current_Bid_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Ask_Total : Harriet.Money.Money_Type)
      return Harriet.Quantities.Quantity_Type;

   function Historical_Supply
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Since     : Duration)
      return Harriet.Quantities.Quantity_Type;

   function Historical_Supply
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Max_Price : Harriet.Money.Price_Type;
      Since     : Duration)
      return Harriet.Quantities.Quantity_Type;

   function Historical_Demand
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Since     : Duration)
      return Harriet.Quantities.Quantity_Type;

   function Historical_Demand
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Min_Price : Harriet.Money.Price_Type;
      Since     : Duration)
      return Harriet.Quantities.Quantity_Type;

   procedure Create_Ask
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type);

   procedure Create_Bid
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type);

   function Previous_Ask
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Previous_Ask_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Money.Price_Type;

   function Previous_Bid
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Remaining_Ask
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Remaining_Bid
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Has_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Boolean;

   function Stock_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Stock_Value
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Money.Money_Type;

   function Stock_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Money.Price_Type;

   procedure Add_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Value     : Harriet.Money.Money_Type);

   procedure Remove_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type);

   procedure Scan_Stock
     (Manager : Root_Agent_Manager_Type'Class;
      Process : not null access
        procedure (Commodity : Harriet.Commodities.Commodity_Reference;
                   Quantity  : Harriet.Quantities.Quantity_Type;
                   Value     : Harriet.Money.Money_Type));

   procedure Initialize_Agent_Manager
     (Manager           : in out Root_Agent_Manager_Type'Class;
      Agent             : Harriet.Db.Agent.Agent_Type;
      Market            : Harriet.Markets.Harriet_Market;
      Planning_Cycle    : Positive);

   procedure Log
     (Manager : Root_Agent_Manager_Type'Class;
      Message : String);

private

   type Root_Agent_Manager_Type is
     abstract new Root_Manager_Type with
      record
         Agent             : Harriet.Db.Agent_Reference;
         Has_Stock         : Harriet.Db.Has_Stock_Reference;
         Account           : Harriet.Db.Account_Reference;
         Market            : Harriet.Markets.Harriet_Market;
         Planning_Cycle    : Positive;
         Update_Count      : Natural := 0;
         Last_Earn         : Harriet.Money.Money_Type :=
           Harriet.Money.Zero;
         Last_Spend        : Harriet.Money.Money_Type :=
           Harriet.Money.Zero;
      end record;

end Harriet.Managers.Agents;
