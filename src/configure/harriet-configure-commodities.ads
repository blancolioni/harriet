with Tropos;

with Harriet.Db.Has_Stock;

package Harriet.Configure.Commodities is

   procedure Configure_Commodities
     (Scenario_Name : String);

   procedure Configure_Stock
     (Has_Stock : Harriet.Db.Has_Stock.Has_Stock_Type;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0);

   procedure Configure_Stock
     (Has_Stock : Harriet.Db.Has_Stock_Reference;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0);

   procedure Configure_Constructed
     (Constructed : Harriet.Db.Constructed_Reference;
      Config      : Tropos.Configuration;
      Factor      : Non_Negative_Real := 1.0);

   procedure Configure_Supplied
     (Supplied : Harriet.Db.Supplied_Reference;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0);

end Harriet.Configure.Commodities;
