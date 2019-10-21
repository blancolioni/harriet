with Tropos.Reader;

with Harriet.Commodities;
with Harriet.Configure.Commodities;

with Harriet.Db.Facility;
with Harriet.Db.Facility_Input;
with Harriet.Db.Facility_Output;

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
      Facility  : constant Harriet.Db.Facility_Reference :=
        Harriet.Db.Facility.Create
          (Enabled_By     => Harriet.Db.Null_Technology_Reference,
           Tag            => Config.Config_Name,
           Operating_Cost =>
             Harriet.Money.To_Money (Config.Get ("operating-cost")),
           Employees      =>
             Harriet.Quantities.To_Quantity (Config.Get ("employees")),
           Power          => Config.Get ("power", 0),
           Generate       => Config.Get ("generate", 0),
           Strip_Mining   => Config.Get ("strip-mine", 0),
           Mining         => Config.Get ("mine", 0),
           Industry       => Config.Get ("industry", 0),
           Research       => Config.Get ("research", 0));
   begin
      Harriet.Configure.Commodities.Configure_Constructed
        (Constructed => Harriet.Db.Facility.Get (Facility)
         .Get_Constructed_Reference,
         Config      => Config);

      for Input_Config of Config.Child ("inputs") loop
         declare
            Tag : constant String := Input_Config.Config_Name;
            Commodity : constant Harriet.Commodities.Commodity_Reference :=
              (if Harriet.Commodities.Exists (Tag)
               then Harriet.Commodities.Get (Tag)
               else raise Constraint_Error with
                 "in input configuration for "
               & Config.Config_Name
               & ": no such commodity: " & Tag);
         begin
            Harriet.Db.Facility_Input.Create
              (Facility  => Facility,
               Commodity => Commodity,
               Quantity  =>
                 Harriet.Quantities.To_Quantity
                   (Input_Config.Value));
         end;
      end loop;

      for Output_Config of Config.Child ("outputs") loop
         declare
            Tag       : constant String := Output_Config.Config_Name;
            Commodity : constant Harriet.Commodities.Commodity_Reference :=
              (if Harriet.Commodities.Exists (Tag)
               then Harriet.Commodities.Get (Tag)
               else raise Constraint_Error with
                 "in output configuration for "
               & Config.Config_Name
               & ": no such commodity: " & Tag);
         begin
            Harriet.Db.Facility_Output.Create
              (Facility  => Facility,
               Commodity => Commodity,
               Quantity  =>
                 Harriet.Quantities.To_Quantity
                   (Output_Config.Value));
         end;
      end loop;

   end Configure_Facility;

end Harriet.Configure.Facilities;
