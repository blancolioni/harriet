with Ada.Containers.Indefinite_Vectors;

with Tropos.Reader;

with Harriet.Commodities;
with Harriet.Random;

with Harriet.Db.Commodity;
with Harriet.Db.Construction_Input;
with Harriet.Db.Input_Commodity;
with Harriet.Db.Manufactured;
with Harriet.Db.Resource;
with Harriet.Db.Resource_Sphere;
with Harriet.Db.Stock_Item;
with Harriet.Db.Supply_Input;

package body Harriet.Configure.Commodities is

   type Resource_Availability is
      record
         Reference : Harriet.Db.Resource_Reference;
         Frequency : Unit_Real;
      end record;

   type Available_Resources is
     array (Positive range <>) of Resource_Availability;

   procedure Configure_Resources
     (Config         : Tropos.Configuration);

   procedure Configure_Non_Resources
     (Commodity_Config : Tropos.Configuration);

   procedure Configure_Resource_Spheres
     (Config    : Tropos.Configuration;
      Available : Available_Resources);

   ---------------------------
   -- Configure_Commodities --
   ---------------------------

   procedure Configure_Commodities (Scenario_Name : String) is
   begin
      Configure_Resources
        (Tropos.Reader.Read_Config
           (Scenario_File
                (Scenario_Name, "commodities", "resource.commodity")));
      Configure_Non_Resources
        (Tropos.Reader.Read_Config
           (Scenario_File
                (Scenario_Name, "commodities", "water.commodity")));
      Configure_Non_Resources
        (Tropos.Reader.Read_Config
           (Scenario_File
                (Scenario_Name, "commodities", "power.commodity")));
      Configure_Non_Resources
        (Tropos.Reader.Read_Config
           (Scenario_File
                (Scenario_Name, "commodities", "refined.commodity")));
      Configure_Non_Resources
        (Tropos.Reader.Read_Config
           (Scenario_File
                (Scenario_Name, "commodities", "manufactured.commodity")));
   end Configure_Commodities;

   ---------------------------
   -- Configure_Constructed --
   ---------------------------

   procedure Configure_Constructed
     (Constructed : Harriet.Db.Constructed_Reference;
      Config      : Tropos.Configuration;
      Factor      : Non_Negative_Real := 1.0)
   is
      Stock_Config : constant Tropos.Configuration :=
        (if Config.Contains ("build")
         then Config.Child ("build")
         else Config);
   begin
      for Item_Config of Stock_Config loop
         if Harriet.Commodities.Exists (Item_Config.Config_Name) then
            declare
               Commodity : constant Harriet.Commodities.Commodity_Reference :=
                 Harriet.Commodities.Get (Item_Config.Config_Name);
               Quantity  : constant Harriet.Quantities.Quantity_Type :=
                 Harriet.Quantities.Scale
                   (Harriet.Quantities.To_Quantity
                      (Real (Float'(Item_Config.Value))),
                    Factor);
            begin
               Harriet.Db.Construction_Input.Create
                 (Constructed => Constructed,
                  Commodity   => Commodity,
                  Quantity    => Quantity);
            end;
         else
            raise Constraint_Error with
              "no such commodity in stock configuration: "
              & Item_Config.Config_Name;
         end if;
      end loop;
   end Configure_Constructed;

   -----------------------------
   -- Configure_Non_Resources --
   -----------------------------

   procedure Configure_Non_Resources
     (Commodity_Config  : Tropos.Configuration)
   is
   begin
      for Item_Config of Commodity_Config loop
         Harriet.Db.Manufactured.Create
           (Enabled_By => Harriet.Db.Null_Technology_Reference,
            Tag        => Item_Config.Config_Name,
            Mass       => Item_Config.Get ("mass", 1.0));
      end loop;

      for Item_Config of Commodity_Config loop
         declare
            Item : constant Harriet.Db.Manufactured_Reference :=
                     Harriet.Db.Manufactured.Get_Reference_By_Tag
                       (Item_Config.Config_Name);
         begin
            for Input_Config of Item_Config.Child ("parts") loop
               declare
                  use Harriet.Commodities;
                  Tag      : constant String := Input_Config.Config_Name;
                  Input    : constant Commodity_Reference :=
                               (if Exists (Tag)
                                then Get (Tag)
                                else raise Constraint_Error with
                                Item_Config.Config_Name
                                & ": no such input commodity: " & Tag);
                  Quantity : constant Harriet.Quantities.Quantity_Type :=
                               Harriet.Quantities.To_Quantity
                                 (Input_Config.Value);
               begin
                  Harriet.Db.Input_Commodity.Create
                    (Manufactured => Item,
                     Commodity    => Input,
                     Quantity     => Quantity);
               end;
            end loop;
         end;
      end loop;
   end Configure_Non_Resources;

   --------------------------------
   -- Configure_Resource_Spheres --
   --------------------------------

   procedure Configure_Resource_Spheres
     (Config    : Tropos.Configuration;
      Available : Available_Resources)
   is
      Radius : constant Tropos.Configuration := Config.Child ("radius");
      RX     : constant Real := Real (Float'(Radius.Get (1)));
      RY     : constant Real := Real (Float'(Radius.Get (2)));
      RZ     : constant Real := Real (Float'(Radius.Get (3)));
      S_Min  : constant Real :=
                 Config.Child ("strength").Get (1);
      S_Max  : constant Real :=
                 Config.Child ("strength").Get (2);
      A_Min  : constant Real :=
                 Config.Child ("attenuation").Get (1);
      A_Max  : constant Real :=
                 Config.Child ("attenuation").Get (2);
      Total  : Non_Negative_Real := 0.0;
   begin
      for R of Available loop
         Total := Total + R.Frequency;
      end loop;

      for I in 1 .. Config.Get ("count") loop
         declare
            X     : constant Real :=
                      RX * (Harriet.Random.Unit_Random * 2.0 - 1.0);
            Y     : constant Real :=
                      RY * (Harriet.Random.Unit_Random * 2.0 - 1.0);
            Z     : constant Real :=
                      RZ * (Harriet.Random.Unit_Random * 2.0 - 1.0);
            S     : constant Real :=
                      S_Min + Harriet.Random.Unit_Random * (S_Max - S_Min);
            R     : constant Real :=
                      Real'Max
                        (Harriet.Random.Normal_Random (0.1) * S, RX / 20.0);
            A     : constant Real :=
                      A_Min + Harriet.Random.Unit_Random * (A_Max - A_Min);
            F     : Non_Negative_Real :=
                      Harriet.Random.Unit_Random * Total;
            Index : Positive := 1;
         begin
            while F > Available (Index).Frequency loop
               F := F - Available (Index).Frequency;
               Index := Index + 1;
            end loop;

            Harriet.Db.Resource_Sphere.Create
              (Resource    => Available (Index).Reference,
               Centre_X    => X,
               Centre_Y    => Y,
               Centre_Z    => Z,
               Strength    => S * Available (Index).Frequency,
               Radius      => R,
               Attenuation => A);
         end;
      end loop;
   end Configure_Resource_Spheres;

   -------------------------
   -- Configure_Resources --
   -------------------------

   procedure Configure_Resources
     (Config         : Tropos.Configuration)
   is
      package Name_Vectors is
        new Ada.Containers.Indefinite_Vectors (Positive, String);
      Names : Name_Vectors.Vector;
   begin

      Harriet.Db.Commodity.Create
        ("raw-resources", Mass => 1.0);

      for Name_Config of Config.Child ("names") loop
         Names.Append (Name_Config.Config_Name);
      end loop;

      declare
         Refs : Available_Resources (1 .. Names.Last_Index);
         Freq : Unit_Real := 1.0;
      begin
         for I in Refs'Range loop
            Refs (I) :=
              Resource_Availability'
                (Reference =>
                   Harriet.Db.Resource.Create
                     (Tag             => Names.Element (I),
                      Name            => Names.Element (I),
                      Mass            => 1.0,
                      Is_Raw_Resource => False),
                 Frequency => Freq);
            Freq := Freq * 0.9;
         end loop;

         Configure_Resource_Spheres (Config.Child ("spheres"), Refs);
      end;

   end Configure_Resources;

   ---------------------
   -- Configure_Stock --
   ---------------------

   procedure Configure_Stock
     (Has_Stock : Harriet.Db.Has_Stock.Has_Stock_Type;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0)
   is
   begin
      Configure_Stock (Has_Stock.Get_Has_Stock_Reference, Config, Factor);
   end Configure_Stock;

   ---------------------
   -- Configure_Stock --
   ---------------------

   procedure Configure_Stock
     (Has_Stock : Harriet.Db.Has_Stock_Reference;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0)
   is
      Stock_Config : constant Tropos.Configuration :=
                       (if Config.Contains ("stock")
                        then Config.Child ("stock")
                        else Config);
   begin
      for Item_Config of Stock_Config loop
         if Harriet.Commodities.Exists (Item_Config.Config_Name) then
            declare
               Commodity : constant Harriet.Commodities.Commodity_Reference :=
                 Harriet.Commodities.Get (Item_Config.Config_Name);
               Quantity  : constant Harriet.Quantities.Quantity_Type :=
                 Harriet.Quantities.Scale
                   (Harriet.Quantities.To_Quantity
                      (Real (Float'(Item_Config.Value))),
                    Factor);
            begin
               Harriet.Db.Stock_Item.Create
                 (Has_Stock => Has_Stock,
                  Commodity => Commodity,
                  Quantity  => Quantity);
            end;
         else
            raise Constraint_Error with
              "no such commodity in stock configuration: "
              & Item_Config.Config_Name;
         end if;
      end loop;
   end Configure_Stock;

   ------------------------
   -- Configure_Supplied --
   ------------------------

   procedure Configure_Supplied
     (Supplied : Harriet.Db.Supplied_Reference;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0)
   is
      Stock_Config : constant Tropos.Configuration :=
        (if Config.Contains ("supply")
         then Config.Child ("supply")
         else Config);
   begin
      for Item_Config of Stock_Config loop
         if Harriet.Commodities.Exists (Item_Config.Config_Name) then
            declare
               Commodity : constant Harriet.Commodities.Commodity_Reference :=
                 Harriet.Commodities.Get (Item_Config.Config_Name);
               Quantity  : constant Harriet.Quantities.Quantity_Type :=
                 Harriet.Quantities.Scale
                   (Harriet.Quantities.To_Quantity
                      (Real (Float'(Item_Config.Value))),
                    Factor);
            begin
               Harriet.Db.Supply_Input.Create
                 (Supplied    => Supplied,
                  Commodity   => Commodity,
                  Quantity    => Quantity);
            end;
         else
            raise Constraint_Error with
              "no such commodity in stock configuration: "
              & Item_Config.Config_Name;
         end if;
      end loop;
   end Configure_Supplied;

end Harriet.Configure.Commodities;
