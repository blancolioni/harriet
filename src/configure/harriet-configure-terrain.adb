with Tropos.Reader;

with Harriet.Color;

with Harriet.Db.Resource;
with Harriet.Db.Terrain;
with Harriet.Db.Terrain_Resource;

package body Harriet.Configure.Terrain is

   procedure Configure_Terrain
     (Config : Tropos.Configuration);

   -----------------------
   -- Configure_Terrain --
   -----------------------

   procedure Configure_Terrain
     (Scenario_Name : String)
   is
   begin
      Tropos.Reader.Read_Config
        (Path      => Scenario_Directory (Scenario_Name, "terrain"),
         Extension => "terrain",
         Configure => Configure_Terrain'Access);
   end Configure_Terrain;

   -----------------------
   -- Configure_Terrain --
   -----------------------

   procedure Configure_Terrain
     (Config : Tropos.Configuration)
   is
      Color : constant Harriet.Color.Harriet_Color :=
                Harriet.Color.From_String
                  (Config.Get ("color", "#000"));
      Terrain : constant Harriet.Db.Terrain_Reference :=
                  Harriet.Db.Terrain.Create
                    (Tag      => Config.Config_Name,
                     Red      => Color.Red,
                     Green    => Color.Green,
                     Blue     => Color.Blue,
                     Hazard   => Get_Real (Config, "hazard") / 100.0,
                     Is_Water => Config.Get ("is_water"));
   begin
      for Resource_Config of Config.Child ("resource") loop
         declare
            use Harriet.Db;
            Resource : constant Resource_Reference :=
              Harriet.Db.Resource.Get_Reference_By_Tag
                (Resource_Config.Config_Name);
            Chance   : constant Natural :=
              Resource_Config.Value;
         begin
            if Resource = Null_Resource_Reference then
               raise Constraint_Error with
                 "in terrain " & Config.Config_Name
                 & ": unknown resource: " & Resource_Config.Config_Name;
            end if;

            Harriet.Db.Terrain_Resource.Create
              (Terrain  => Terrain,
               Resource => Resource,
               Chance   => Real (Chance) / 100.0);
         end;
      end loop;
   end Configure_Terrain;

end Harriet.Configure.Terrain;
