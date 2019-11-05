with WL.Images.Bitmaps;

with Tropos.Reader;

with Harriet.Color;

with Harriet.Db.Climate_Terrain;
with Harriet.Db.Elevation;
with Harriet.Db.Terrain;

with Harriet.Paths;

package body Harriet.Configure.Terrain is

   Min_Height : Integer := Integer'Last;
   Max_Height : Integer := Integer'First;

   procedure Configure_Terrain
     (Config : Tropos.Configuration);

   procedure Configure_Climate_Terrain
     (Config : Tropos.Configuration);

   procedure Configure_Elevation;

   -------------------------------
   -- Configure_Climate_Terrain --
   -------------------------------

   procedure Configure_Climate_Terrain
     (Config : Tropos.Configuration)
   is
      Climate : constant Harriet.Db.World_Climate :=
        Harriet.Db.World_Climate'Value (Config.Config_Name);
      Next    : Natural := 0;
   begin
      for Terrain_Config of Config.Child ("terrain") loop
         declare
            Terrain : constant Harriet.Db.Terrain_Reference :=
                        Harriet.Db.Terrain.Get_Reference_By_Tag
                          (Terrain_Config.Config_Name);
            Frequency : constant Real := Terrain_Config.Value;
         begin
            Next := Next + 1;
            Harriet.Db.Climate_Terrain.Create
              (Climate   => Climate,
               Terrain   => Terrain,
               Sequence  => Next,
               Frequency => Frequency);
         end;
      end loop;

   end Configure_Climate_Terrain;

   -------------------------
   -- Configure_Elevation --
   -------------------------

   procedure Configure_Elevation is
      Palette_File_Name : constant String :=
        Harriet.Paths.Config_File
          ("star-systems/palettes/land-elevation.bmp");
      Reader            : WL.Images.Bitmaps.Bitmap_Image_Reader;
      Image             : WL.Images.Image_Type;
      Water_Color       : Harriet.Color.Harriet_Color;
   begin
      for Terrain of Harriet.Db.Terrain.Scan_By_Tag loop
         if Terrain.Is_Water then
            Water_Color := (Terrain.Red, Terrain.Green, Terrain.Blue, 1.0);
         end if;
      end loop;

      for I in Min_Height .. 0 loop
         Harriet.Db.Elevation.Create
           (Red     => Water_Color.Red,
            Green   => Water_Color.Green,
            Blue    => Water_Color.Blue,
            Height  => I);
      end loop;

      Reader.Read (Palette_File_Name, Image);

      for X in 1 .. Image.Width loop
         declare
            Color : constant WL.Images.Image_Color :=
              Image.Color (X, 1);
         begin
            Harriet.Db.Elevation.Create
              (Red     => Real (Color.Red) / 255.0,
               Green   => Real (Color.Green) / 255.0,
               Blue    => Real (Color.Blue) / 255.0,
               Height  => Positive (X));
         end;
      end loop;
   end Configure_Elevation;

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
      Configure_Elevation;
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
      Min   : constant Integer := Config.Get ("min-height");
      Max   : constant Integer := Config.Get ("max-height");
   begin
      Min_Height := Integer'Min (Min_Height, Min);
      Max_Height := Integer'Max (Max_Height, Max);
      Harriet.Db.Terrain.Create
        (Tag      => Config.Config_Name,
         Red      => Color.Red,
         Green    => Color.Green,
         Blue     => Color.Blue,
         Hazard   => Get_Real (Config, "hazard") / 100.0,
         Is_Water => Config.Get ("is-water"));
   end Configure_Terrain;

end Harriet.Configure.Terrain;
