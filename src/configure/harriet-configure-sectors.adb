with Tropos.Reader;

with Harriet.Db.Sector_Use;

package body Harriet.Configure.Sectors is

   procedure Configure_Sector_Use
     (Config : Tropos.Configuration);

   --------------------------
   -- Configure_Sector_Use --
   --------------------------

   procedure Configure_Sector_Use (Scenario_Name : String) is
   begin
      for Config of
        Tropos.Reader.Read_Config
          (Path      =>
             Harriet.Configure.Scenario_Directory
               (Scenario_Name, "sectors"),
           Extension => "sector")
      loop
         Configure_Sector_Use (Config);
      end loop;
   end Configure_Sector_Use;

   --------------------------
   -- Configure_Sector_Use --
   --------------------------

   procedure Configure_Sector_Use
     (Config : Tropos.Configuration)
   is
   begin
      Harriet.Db.Sector_Use.Create
        (Tag => Config.Config_Name);
   end Configure_Sector_Use;

end Harriet.Configure.Sectors;
