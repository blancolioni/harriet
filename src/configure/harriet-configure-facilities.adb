with Tropos.Reader;

with Harriet.Commodities;
with Harriet.Configure.Commodities;

with Harriet.Db.Facility;
with Harriet.Db.Facility_Input;
with Harriet.Db.Facility_Output;
with Harriet.Db.Mining_Facility;
with Harriet.Db.Resource;

package body Harriet.Configure.Facilities is

   procedure Configure_Facility
     (Config : Tropos.Configuration);

   function Create_Facility
     (Config : Tropos.Configuration)
      return Harriet.Db.Facility_Reference;

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
                    Create_Facility (Config);
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

   ---------------------
   -- Create_Facility --
   ---------------------

   function Create_Facility
     (Config : Tropos.Configuration)
      return Harriet.Db.Facility_Reference
   is
   begin
      if Config.Get ("mine", 0) > 0 then
         declare
            Ref : constant Harriet.Db.Mining_Facility_Reference :=
                    Harriet.Db.Mining_Facility.Create
                      (Enabled_By         =>
                                 Harriet.Db.Null_Technology_Reference,
                       Tag                => Config.Config_Name,
                       Operating_Cost     =>
                         Harriet.Money.To_Money
                           (Config.Get ("operating-cost")),
                       Employees          =>
                         Harriet.Quantities.To_Quantity
                           (Config.Get ("employees")),
                       Power              =>
                         Config.Get ("power", 0),
                       Generate           =>
                         Config.Get ("generate", 0),
                       Free_Installations =>
                         Config.Get ("free-installations", 0),
                       Strip_Mining       =>
                         Config.Get ("strip-mine", 0),
                       Mining             =>
                         Config.Get ("mine", 0),
                       Industry           =>
                         Config.Get ("industry", 0),
                       Research           =>
                         Config.Get ("research", 0),
                       Resource           =>
                         Harriet.Db.Resource.Get_Reference_By_Tag
                           (Config.Get ("resource")));

         begin
            return Harriet.Db.Mining_Facility.Get (Ref).Get_Facility_Reference;
         end;
      else
         return Harriet.Db.Facility.Create
           (Enabled_By         => Harriet.Db.Null_Technology_Reference,
            Tag                => Config.Config_Name,
            Operating_Cost     =>
              Harriet.Money.To_Money (Config.Get ("operating-cost")),
            Employees          =>
              Harriet.Quantities.To_Quantity (Config.Get ("employees")),
            Power              => Config.Get ("power", 0),
            Generate           => Config.Get ("generate", 0),
            Free_Installations => Config.Get ("free-installations", 0),
            Strip_Mining       => Config.Get ("strip-mine", 0),
            Mining             => Config.Get ("mine", 0),
            Industry           => Config.Get ("industry", 0),
            Research           => Config.Get ("research", 0));
      end if;
   end Create_Facility;

end Harriet.Configure.Facilities;
