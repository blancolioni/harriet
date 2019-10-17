with Tropos.Reader;

with Harriet.Commodities;

with Harriet.Db.Commodity_Class;
with Harriet.Db.Production;
with Harriet.Db.Input_Item;
with Harriet.Db.Output_Item;
with Harriet.Db.Efficiency_Item;
with Harriet.Db.Required_Item;
with Harriet.Db.Zone;

package body Harriet.Configure.Production is

   procedure Configure_Production
     (Production_Config : Tropos.Configuration);

   procedure Configure_Production_Items
     (Production : Harriet.Db.Production_Reference;
      Config     : Tropos.Configuration;
      Create : not null access
        procedure (Production : Harriet.Db.Production_Reference;
                   Commodity  : Harriet.Db.Commodity_Reference;
                   Category   : Harriet.Db.Commodity_Class_Reference;
                   Quantity   : Harriet.Quantities.Quantity_Type));

   --------------------------
   -- Configure_Production --
   --------------------------

   procedure Configure_Production
     (Scenario_Name : String)
   is
   begin
      for Production_Config of
        Tropos.Reader.Read_Config
          (Scenario_Directory (Scenario_Name, "production"),
           "production")
      loop
         Configure_Production (Production_Config);
      end loop;
   end Configure_Production;

   --------------------------
   -- Configure_Production --
   --------------------------

   procedure Configure_Production
     (Production_Config : Tropos.Configuration)
   is
      use type Harriet.Db.Zone_Reference;
      Zone : constant Harriet.Db.Zone_Reference :=
        Harriet.Db.Zone.Get_Reference_By_Tag
          (Production_Config.Get ("zone", ""));
      Size : constant Non_Negative_Real :=
        Real (Float'(Production_Config.Get ("size", 1.0)));
      P : constant Harriet.Db.Production_Reference :=
        Harriet.Db.Production.Create
          (Tag  => Production_Config.Config_Name,
           Zone => Zone,
           Size => Size);
   begin

      if Zone = Harriet.Db.Null_Zone_Reference then
         raise Constraint_Error with
           "in production " & Production_Config.Config_Name
           & ": no such zone: "
           & Production_Config.Get ("zone");
      end if;

      Configure_Production_Items
        (Production => P,
         Config     => Production_Config.Child ("in"),
         Create     => Harriet.Db.Input_Item.Create'Access);
      Configure_Production_Items
        (Production => P,
         Config     => Production_Config.Child ("out"),
         Create     => Harriet.Db.Output_Item.Create'Access);
      Configure_Production_Items
        (Production => P,
         Config     => Production_Config.Child ("efficiency"),
         Create     => Harriet.Db.Efficiency_Item.Create'Access);
      Configure_Production_Items
        (Production => P,
         Config     => Production_Config.Child ("with"),
         Create     => Harriet.Db.Required_Item.Create'Access);
   end Configure_Production;

   --------------------------------
   -- Configure_Production_Items --
   --------------------------------

   procedure Configure_Production_Items
     (Production : Harriet.Db.Production_Reference;
      Config     : Tropos.Configuration;
      Create     : not null access
        procedure (Production : Harriet.Db.Production_Reference;
                   Commodity  : Harriet.Db.Commodity_Reference;
                   Category   : Harriet.Db.Commodity_Class_Reference;
                   Quantity   : Harriet.Quantities.Quantity_Type))
   is
   begin
      for Item of Config loop
         if Harriet.Commodities.Exists (Item.Config_Name) then
            Create (Production,
                    Harriet.Commodities.To_Database_Reference
                      (Harriet.Commodities.Get (Item.Config_Name)),
                    Harriet.Db.Null_Commodity_Class_Reference,
                    Harriet.Quantities.To_Quantity
                      (Real (Float'(Item.Value))));
         else
            declare
               use type Harriet.Db.Commodity_Class_Reference;
               Class : constant Harriet.Db.Commodity_Class_Reference :=
                 Harriet.Db.Commodity_Class.Get_Reference_By_Tag
                   (Item.Config_Name);
            begin
               if Class /= Harriet.Db.Null_Commodity_Class_Reference then
                  Create (Production, Harriet.Db.Null_Commodity_Reference,
                          Class,
                          Harriet.Quantities.To_Quantity
                            (Real (Float'(Item.Value))));
               else
                  raise Constraint_Error with
                    "no such commodity or class: " & Item.Config_Name;
               end if;
            end;
         end if;
      end loop;
   end Configure_Production_Items;

end Harriet.Configure.Production;
