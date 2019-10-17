with Harriet.Options;

with Harriet.Configure.Commodities;
with Harriet.Configure.Facilities;
with Harriet.Configure.Galaxies;
with Harriet.Configure.Ships;

with Harriet.Db.Scenario;

package body Harriet.Configure.Scenarios is

   -------------------
   -- Load_Scenario --
   -------------------

   procedure Load_Scenario
     (Scenario_Name  : String;
      Name_Generator : WL.Random.Names.Name_Generator)
   is
      R  : constant Natural := Harriet.Options.Galaxy_Radius;
      RX : constant Natural := Harriet.Options.Galaxy_Radius_X;
      RY : constant Natural := Harriet.Options.Galaxy_Radius_Y;
      RZ : constant Natural := Harriet.Options.Galaxy_Radius_Z;
   begin
      Harriet.Db.Scenario.Create
        (Scenario_Name, True, Harriet.Db.Null_Star_System_Reference);

      Harriet.Configure.Commodities.Configure_Commodities
        (Scenario_Name);
      Harriet.Configure.Facilities.Configure_Facilities
        (Scenario_Name);
      Harriet.Configure.Ships.Configure_Ship_Components (Scenario_Name);
      Harriet.Configure.Ships.Configure_Ship_Designs (Scenario_Name);

      Harriet.Configure.Galaxies.Generate_Galaxy
        (Number_Of_Systems  => Harriet.Options.System_Count,
         Radius_X           => Real (if RX = 0 then R else RX),
         Radius_Y           =>
           Real (if RY = 0 then (if RX = 0 then R else RX) else RY),
         Radius_Z           =>
           Real (if RZ = 0 then (if RX = 0 then R else RX) else RZ),
         Create_Coordinates =>
           Harriet.Configure.Galaxies.Random_Sphere_Distribution'Access,
         Names              => Name_Generator);

   end Load_Scenario;

end Harriet.Configure.Scenarios;
