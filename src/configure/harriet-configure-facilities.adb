with Tropos.Reader;

with Harriet.Commodities;
with Harriet.Configure.Commodities;

with Harriet.Db.Facility;

package body Harriet.Configure.Facilities is

   procedure Configure_Facility
     (Config : Tropos.Configuration);

   --------------------------
   -- Configure_Facilities --
   --------------------------

   procedure Configure_Facilities
     (Scenario_Name : String)
   is
   begin
      for Installation_Config of
        Tropos.Reader.Read_Config
          (Path      =>
             Scenario_Directory (Scenario_Name, "facilities"),
           Extension => "facility")
      loop
         Configure_Facility (Installation_Config);
      end loop;
   end Configure_Facilities;

   ------------------------
   -- Configure_Facility --
   ------------------------

   procedure Configure_Facility
     (Config : Tropos.Configuration)
   is
      Generator : constant Boolean := Config.Get ("generator");
      Facility  : constant Harriet.Db.Facility_Reference :=
        Harriet.Db.Facility.Create
          (Enabled_By     => Harriet.Db.Null_Technology_Reference,
           Tag            => Config.Config_Name,
           Operating_Cost =>
             Harriet.Money.To_Money (Config.Get ("operating-cost")),
           Generator      => Generator,
           Consumption    =>
             Harriet.Quantities.To_Quantity
               (Config.Get ("consumption", 0.0)),
           Commodity      =>
             (if Config.Contains ("consume")
              then Harriet.Commodities.Get (Config.Get ("consume"))
              else Harriet.Db.Null_Commodity_Reference),
           Employees      =>
             Harriet.Quantities.To_Quantity (Config.Get ("employees")),
           Power          => Config.Get ("power", 0),
           Mining         => Config.Get ("mine", 0),
           Industry       => Config.Get ("industry", 0),
           Research       => Config.Get ("research", 0));
   begin
      Harriet.Configure.Commodities.Configure_Constructed
        (Constructed => Harriet.Db.Facility.Get (Facility)
         .Get_Constructed_Reference,
         Config      => Config);
   end Configure_Facility;

end Harriet.Configure.Facilities;
