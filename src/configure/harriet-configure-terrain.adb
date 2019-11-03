with Tropos.Reader;

with Harriet.Color;

with Harriet.Db.Climate_Terrain;
with Harriet.Db.Terrain;

package body Harriet.Configure.Terrain is

   procedure Configure_Terrain
     (Config : Tropos.Configuration);

   procedure Configure_Climate_Terrain
     (Config : Tropos.Configuration);

   -------------------------------
   -- Configure_Climate_Terrain --
   -------------------------------

   procedure Configure_Climate_Terrain
     (Config : Tropos.Configuration)
   is
      Climate : constant Harriet.Db.World_Climate :=
                  Harriet.Db.World_Climate'Value (Config.Config_Name);
   begin
      for Terrain_Config of Config.Child ("terrain") loop
         declare
            Terrain : constant Harriet.Db.Terrain_Reference :=
                        Harriet.Db.Terrain.Get_Reference_By_Tag
                          (Terrain_Config.Config_Name);
            Frequency : constant Natural := Terrain_Config.Value;
         begin
            Harriet.Db.Climate_Terrain.Create
              (Climate   => Climate,
               Terrain   => Terrain,
               Frequency => Frequency);
         end;
      end loop;

   end Configure_Climate_Terrain;

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
      Tropos.Reader.Read_Config
        (Path      => Scenario_Directory (Scenario_Name, "climate"),
         Extension => "climate",
         Configure => Configure_Climate_Terrain'Access);

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
   begin
      Harriet.Db.Terrain.Create
        (Tag      => Config.Config_Name,
         Red      => Color.Red,
         Green    => Color.Green,
         Blue     => Color.Blue,
         Hazard   => Get_Real (Config, "hazard") / 100.0,
         Is_Water => Config.Get ("is-water"));
   end Configure_Terrain;

end Harriet.Configure.Terrain;
