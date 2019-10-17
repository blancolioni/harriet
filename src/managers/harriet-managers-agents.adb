with Ada.Containers.Doubly_Linked_Lists;

with WL.Random;

with Harriet.Agents;
with Harriet.Random;
with Harriet.Stock;

with Harriet.Db.Account;
with Harriet.Db.Lease_Contract;
with Harriet.Db.Market_Offer;
with Harriet.Db.Stock_Item;

package body Harriet.Managers.Agents is

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Agent_Manager_Type)
   is
      M : Root_Agent_Manager_Type'Class renames
            Root_Agent_Manager_Type'Class (Manager.all);
   begin
      Manager.Log
        ("activating: cash = "
         & Harriet.Money.Show
           (Harriet.Agents.Cash (M.Account)));

      M.On_Activation_Begin;

      if Manager.Update_Count mod Manager.Planning_Cycle = 0 then
         M.Create_Planning;
      end if;

      M.Create_Bids;
      M.Execute_Production;
      M.Execute_Consumption;

      M.On_Activation_End;

      if M.Update_Count = 0 then
         M.Update_Count :=
           WL.Random.Random_Number (1, Manager.Planning_Cycle);
         M.Set_Next_Update_Delay
           (Harriet.Calendar.Days (Harriet.Random.Unit_Random + 0.5));
      else
         M.Update_Count := M.Update_Count + 1;
         M.Set_Next_Update_Delay (Harriet.Calendar.Days (1));
      end if;

   end Activate;

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Value     : Harriet.Money.Money_Type)
   is
   begin
      Harriet.Stock.Add_Stock
        (To       => Manager.Has_Stock,
         Item     => Commodity,
         Quantity => Quantity,
         Value    => Value);
   end Add_Stock;

   ---------------
   -- Available --
   ---------------

   function Available
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Boolean
   is
      use type Harriet.Quantities.Quantity_Type;
   begin
      return Harriet.Markets.Historical_Offer_Quantity
        (Manager.Market, Commodity, Harriet.Db.Ask,
         Harriet.Calendar.Days (10))
        > Harriet.Quantities.Zero;
   end Available;

   ----------------
   -- Create_Ask --
   ----------------

   procedure Create_Ask
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type)
   is
   begin
      Manager.Log
        ("ask: " & Harriet.Commodities.Local_Name (Commodity)
         & ": " & Harriet.Quantities.Show (Quantity)
         & " "
         & " @ " & Harriet.Money.Show (Price)
         & "ea; total "
         & Harriet.Money.Show
           (Harriet.Money.Total (Price, Quantity)));

      Harriet.Markets.Create_Offer
        (Market    => Manager.Market,
         Agent     => Manager.Agent,
         Has_Stock => Manager.Has_Stock,
         Account   => Manager.Account,
         Offer     => Harriet.Db.Ask,
         Commodity => Commodity,
         Quantity  => Quantity,
         Price     => Price);

   end Create_Ask;

   ----------------
   -- Create_Bid --
   ----------------

   procedure Create_Bid
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type)
   is
   begin
      Manager.Log
        ("bid: " & Harriet.Commodities.Local_Name (Commodity)
         & ": " & Harriet.Quantities.Show (Quantity)
         & " @ " & Harriet.Money.Show (Price)
         & "ea; total "
         & Harriet.Money.Show
           (Harriet.Money.Total (Price, Quantity)));

      Harriet.Markets.Create_Offer
        (Market    => Manager.Market,
         Agent     => Manager.Agent,
         Has_Stock => Manager.Has_Stock,
         Account   => Manager.Account,
         Offer     => Harriet.Db.Bid,
         Commodity => Commodity,
         Quantity  => Quantity,
         Price     => Price);

   end Create_Bid;

   -----------------------
   -- Current_Ask_Price --
   -----------------------

   function Current_Ask_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
      return Harriet.Money.Price_Type
   is
   begin
      return Harriet.Markets.Current_Price
        (Manager.Market, Commodity, Harriet.Db.Ask, Quantity);
   end Current_Ask_Price;

   --------------------------
   -- Current_Ask_Quantity --
   --------------------------

   function Current_Ask_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Markets.Current_Quantity
        (Manager.Market, Commodity, Harriet.Db.Ask);
   end Current_Ask_Quantity;

   --------------------------
   -- Current_Ask_Quantity --
   --------------------------

   function Current_Ask_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Bid_Total : Harriet.Money.Money_Type)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Markets.Current_Quantity
        (Manager.Market, Commodity, Harriet.Db.Ask, Bid_Total);
   end Current_Ask_Quantity;

   -----------------------
   -- Current_Bid_Price --
   -----------------------

   function Current_Bid_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
      return Harriet.Money.Price_Type
   is
   begin
      return Harriet.Markets.Current_Price
        (Manager.Market, Commodity, Harriet.Db.Bid, Quantity);
   end Current_Bid_Price;

   --------------------------
   -- Current_Bid_Quantity --
   --------------------------

   function Current_Bid_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Markets.Current_Quantity
        (Manager.Market, Commodity, Harriet.Db.Bid);
   end Current_Bid_Quantity;

   --------------------------
   -- Current_Bid_Quantity --
   --------------------------

   function Current_Bid_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Ask_Total : Harriet.Money.Money_Type)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Markets.Current_Quantity
        (Manager.Market, Commodity, Harriet.Db.Bid, Ask_Total);
   end Current_Bid_Quantity;

   ----------------------
   -- Current_Buy_Cost --
   ----------------------

   function Current_Buy_Cost
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
      return Harriet.Money.Money_Type
   is
   begin
      return Harriet.Markets.Current_Value
        (Manager.Market, Commodity, Harriet.Db.Ask, Quantity);
   end Current_Buy_Cost;

   ------------------
   -- Current_Cash --
   ------------------

   function Current_Cash
     (Manager : Root_Agent_Manager_Type'Class)
      return Harriet.Money.Money_Type
   is
   begin
      return Harriet.Agents.Cash (Manager.Account);
   end Current_Cash;

   -----------------------
   -- Current_Sell_Earn --
   -----------------------

   function Current_Sell_Earn
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
      return Harriet.Money.Money_Type
   is
   begin
      return Harriet.Markets.Current_Value
        (Manager.Market, Commodity, Harriet.Db.Bid, Quantity);
   end Current_Sell_Earn;

   ----------
   -- Earn --
   ----------

   procedure Earn
     (Manager : Root_Agent_Manager_Type'Class;
      Amount  : Harriet.Money.Money_Type;
      Tag     : String)
   is
   begin
      Harriet.Agents.Add_Cash
        (Manager.Account, Amount, Tag);
   end Earn;

   ---------------
   -- Has_Stock --
   ---------------

   function Has_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Boolean
   is
      use type Harriet.Quantities.Quantity_Type;
   begin
      return Harriet.Stock.Get_Quantity (Manager.Has_Stock, Commodity)
        > Harriet.Quantities.Zero;
   end Has_Stock;

   -----------------------
   -- Historical_Demand --
   -----------------------

   function Historical_Demand
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Since     : Duration)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Markets.Historical_Offer_Quantity
        (Market    => Manager.Market,
         Commodity => Commodity,
         Offer     => Harriet.Db.Bid,
         Since     => Since);
   end Historical_Demand;

   -----------------------
   -- Historical_Demand --
   -----------------------

   function Historical_Demand
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Min_Price : Harriet.Money.Price_Type;
      Since     : Duration)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Markets.Historical_Offer_Quantity
        (Market    => Manager.Market,
         Commodity => Commodity,
         Offer     => Harriet.Db.Bid,
         Price     => Min_Price,
         Since     => Since);
   end Historical_Demand;

   ---------------------------
   -- Historical_Mean_Price --
   ---------------------------

   function Historical_Mean_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Money.Price_Type
   is
   begin
      return Harriet.Markets.Historical_Mean_Price
        (Manager.Market, Commodity);
   end Historical_Mean_Price;

   -----------------------
   -- Historical_Supply --
   -----------------------

   function Historical_Supply
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Since     : Duration)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Markets.Historical_Offer_Quantity
        (Market    => Manager.Market,
         Commodity => Commodity,
         Offer     => Harriet.Db.Ask,
         Since     => Since);
   end Historical_Supply;

   -----------------------
   -- Historical_Supply --
   -----------------------

   function Historical_Supply
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Max_Price : Harriet.Money.Price_Type;
      Since     : Duration)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Markets.Historical_Offer_Quantity
        (Market    => Manager.Market,
         Commodity => Commodity,
         Offer     => Harriet.Db.Ask,
         Price     => Max_Price,
         Since     => Since);
   end Historical_Supply;

   ------------------------------
   -- Initialize_Agent_Manager --
   ------------------------------

   procedure Initialize_Agent_Manager
     (Manager           : in out Root_Agent_Manager_Type'Class;
      Agent             : Harriet.Db.Agent.Agent_Type;
      Market            : Harriet.Markets.Harriet_Market;
      Planning_Cycle    : Positive)
   is
   begin
      Manager.Agent := Agent.Get_Agent_Reference;
      Manager.Has_Stock := Agent.Get_Has_Stock_Reference;
      Manager.Account := Agent.Account;
      Manager.Market := Market;
      Manager.Planning_Cycle := Planning_Cycle;
   end Initialize_Agent_Manager;

   ---------------
   -- Last_Earn --
   ---------------

   function Last_Earn
     (Manager : Root_Agent_Manager_Type'Class)
      return Harriet.Money.Money_Type
   is
   begin
      return Manager.Last_Earn;
   end Last_Earn;

   ----------------
   -- Last_Spend --
   ----------------

   function Last_Spend
     (Manager : Root_Agent_Manager_Type'Class)
      return Harriet.Money.Money_Type
   is
   begin
      return Manager.Last_Spend;
   end Last_Spend;

   ---------
   -- Log --
   ---------

   procedure Log
     (Manager : Root_Agent_Manager_Type'Class;
      Message : String)
   is
   begin
      Harriet.Agents.Log_Agent
        (Agent   => Manager.Agent,
         Message => Message);
   end Log;

   -------------------------
   -- On_Activation_Begin --
   -------------------------

   procedure On_Activation_Begin
     (Manager : in out Root_Agent_Manager_Type)
   is
      use type Harriet.Calendar.Time;
   begin
      for Contract of
        Harriet.Db.Lease_Contract.Select_By_Tenant (Manager.Agent)
      loop
         if Contract.Expires > Harriet.Calendar.Clock then
            Manager.Log ("pay " & Harriet.Money.Show (Contract.Daily_Rent)
                         & " to Agent"
                         & Harriet.Db.To_String (Contract.Owner)
                         & " for "
                         & Harriet.Commodities.Local_Name
                           (Harriet.Commodities.Get_Commodity
                              (Contract.Commodity)));
            Manager.Spend (Contract.Daily_Rent, "rent");
            Harriet.Agents.Add_Cash
              (Harriet.Db.Agent.Get (Contract.Owner), Contract.Daily_Rent,
               "rent");
         end if;
      end loop;
      Manager.Last_Earn :=
        Harriet.Db.Account.Get (Manager.Account).Earn;
      Manager.Last_Spend :=
        Harriet.Db.Account.Get (Manager.Account).Spend;
      Manager.Reset_Cashflow;
      Manager.Log ("last period earned "
                   & Harriet.Money.Show (Manager.Last_Earn)
                   & " and spent "
                   & Harriet.Money.Show (Manager.Last_Spend));

   end On_Activation_Begin;

   -----------------------
   -- On_Activation_End --
   -----------------------

   procedure On_Activation_End
     (Manager : in out Root_Agent_Manager_Type)
   is null;

   ------------------
   -- Previous_Ask --
   ------------------

   function Previous_Ask
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Markets.Previous_Agent_Offer
        (Manager.Market, Manager.Agent, Commodity, Harriet.Db.Ask);
   end Previous_Ask;

   ------------------------
   -- Previous_Ask_Price --
   ------------------------

   function Previous_Ask_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Money.Price_Type
   is
   begin
      return Harriet.Markets.Previous_Agent_Offer_Price
        (Manager.Market, Manager.Agent, Commodity, Harriet.Db.Ask);
   end Previous_Ask_Price;

   ------------------
   -- Previous_Bid --
   ------------------

   function Previous_Bid
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Markets.Previous_Agent_Offer
        (Manager.Market, Manager.Agent, Commodity, Harriet.Db.Ask);
   end Previous_Bid;

   -------------------
   -- Remaining_Ask --
   -------------------

   function Remaining_Ask
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Markets.Remaining_Agent_Offer
        (Manager.Market, Manager.Agent, Commodity, Harriet.Db.Ask);
   end Remaining_Ask;

   -------------------
   -- Remaining_Bid --
   -------------------

   function Remaining_Bid
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Markets.Remaining_Agent_Offer
        (Manager.Market, Manager.Agent, Commodity, Harriet.Db.Ask);
   end Remaining_Bid;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
   is
      use type Harriet.Db.Offer_Type;
      Ask : constant Harriet.Db.Market_Offer.Market_Offer_Type :=
        Harriet.Db.Market_Offer.Get_By_Market_Offer
          (Market    => Manager.Market,
           Agent     => Manager.Agent,
           Commodity => Commodities.To_Database_Reference (Commodity));
   begin
      if Ask.Has_Element
        and then Ask.Offer = Harriet.Db.Ask
      then
         declare
            use Harriet.Quantities;
            Ask_Quantity : constant Quantity_Type := Ask.Quantity;
         begin
            Harriet.Db.Market_Offer.Update_Market_Offer
              (Ask.Get_Market_Offer_Reference)
              .Set_Quantity (Ask_Quantity - Min (Quantity, Ask_Quantity))
                .Done;
         end;
      end if;

      Harriet.Stock.Remove_Stock
        (Manager.Has_Stock, Commodity, Quantity);
   end Remove_Stock;

   --------------------
   -- Reset_Cashflow --
   --------------------

   procedure Reset_Cashflow
     (Manager : Root_Agent_Manager_Type'Class)
   is
   begin
      Harriet.Db.Account.Update_Account (Manager.Account)
        .Set_Earn (Harriet.Money.Zero)
        .Set_Spend (Harriet.Money.Zero)
        .Done;
   end Reset_Cashflow;

   ----------------
   -- Scan_Stock --
   ----------------

   procedure Scan_Stock
     (Manager : Root_Agent_Manager_Type'Class;
      Process : not null access
        procedure (Commodity : Harriet.Commodities.Commodity_Reference;
                   Quantity  : Harriet.Quantities.Quantity_Type;
                   Value     : Harriet.Money.Money_Type))
   is
      use Harriet.Quantities;

      type Stock_Item_Record is
         record
            Commodity : Harriet.Db.Commodity_Reference;
            Quantity  : Harriet.Quantities.Quantity_Type;
            Value     : Harriet.Money.Money_Type;
         end record;

      package Stock_Item_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Stock_Item_Record);

      List : Stock_Item_Lists.List;

   begin
      for Stock_Item of
        Harriet.Db.Stock_Item.Select_By_Has_Stock
          (Manager.Has_Stock)
      loop
         if Stock_Item.Quantity > Zero then
            List.Append
              ((Stock_Item.Commodity, Stock_Item.Quantity, Stock_Item.Value));
         end if;
      end loop;

      for Stock_Item of List loop
         Process
           (Harriet.Commodities.Get_Commodity (Stock_Item.Commodity),
            Stock_Item.Quantity, Stock_Item.Value);
      end loop;
   end Scan_Stock;

   -----------
   -- Spend --
   -----------

   procedure Spend
     (Manager : Root_Agent_Manager_Type'Class;
      Amount  : Harriet.Money.Money_Type;
      Tag     : String)
   is
   begin
      Harriet.Agents.Remove_Cash
        (Manager.Account, Amount, Tag);
   end Spend;

   -----------------
   -- Stock_Price --
   -----------------

   function Stock_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Money.Price_Type
   is
   begin
      return Harriet.Stock.Get_Price_Per_Item
        (Manager.Has_Stock, Commodity);
   end Stock_Price;

   --------------------
   -- Stock_Quantity --
   --------------------

   function Stock_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Stock.Get_Quantity (Manager.Has_Stock, Commodity);
   end Stock_Quantity;

   -----------------
   -- Stock_Value --
   -----------------

   function Stock_Value
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Harriet.Commodities.Commodity_Reference)
      return Harriet.Money.Money_Type
   is
   begin
      return Harriet.Stock.Get_Value (Manager.Has_Stock, Commodity);
   end Stock_Value;

end Harriet.Managers.Agents;
