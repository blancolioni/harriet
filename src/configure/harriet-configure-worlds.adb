with Ada.Containers.Vectors;

with WL.Random.Height_Maps;

with Harriet.Logging;
with Harriet.Solar_System;
with Harriet.Surfaces;

with Harriet.Configure.Resources;

with Harriet.Db.Climate_Terrain;
with Harriet.Db.Elevation;
with Harriet.Db.Sector_Neighbour;
with Harriet.Db.Sector_Vertex;
with Harriet.Db.Star_System;
with Harriet.Db.Terrain;
with Harriet.Db.World;
with Harriet.Db.World_Sector;

package body Harriet.Configure.Worlds is

   package Heights renames WL.Random.Height_Maps;

   package Elevation_Vectors is
     new Ada.Containers.Vectors
       (Positive, Harriet.Db.Elevation_Reference, Harriet.Db."=");

   Elevation   : Elevation_Vectors.Vector;
--     Water       : Harriet.Db.Terrain_Reference :=
--                     Harriet.Db.Null_Terrain_Reference;
--     Water_Index : Natural := 0;
   First_Land  : Natural := 0;

   procedure Save_Surface
     (Surface : Harriet.Surfaces.Surface_Type;
      World   : Harriet.Db.World.World_Type);

   function Get_Frequencies
     (World : Harriet.Db.World.World_Type)
      return Heights.Frequency_Map;

   ----------------------
   -- Generate_Surface --
   ----------------------

   procedure Generate_Surface
     (World : Harriet.Db.World_Reference)
   is
      use Harriet.Db;
      Tile_Count : Natural;
      Rec        : constant Harriet.Db.World.World_Type :=
                     Harriet.Db.World.Get (World);
      Radius     : constant Non_Negative_Real :=
                     Rec.Radius
                       / Harriet.Solar_System.Earth_Radius;
   begin

      if Elevation.Is_Empty then
         for E of Harriet.Db.Elevation.Scan_By_Top_Record loop
            Elevation.Append (E.Get_Elevation_Reference);
            if First_Land = 0
              and then E.Height > 0
            then
               First_Land := Elevation.Last_Index;
            end if;
         end loop;

--           for T of Harriet.Db.Terrain.Scan_By_Tag loop
--              if T.Is_Water then
--                 Water := T.Get_Terrain_Reference;
--              end if;
--           end loop;
      end if;

      case Rec.Composition is
         when Ice | Rock | Rock_Ice | Rock_Iron =>
            Tile_Count := Natural (Radius * 400.0);
         when Hydrogen | Gaseous =>
            Tile_Count := 0;
      end case;

      if Tile_Count > 0 then
         declare
            Surface   : Harriet.Surfaces.Root_Surface_Type;
         begin
            Harriet.Logging.Log
              ("surfaces",
               Rec.Name & ": creating surface with "
               & Tile_Count'Image & " tiles");
            Surface.Create_Voronoi_Partition (Tile_Count);
            Save_Surface (Surface, Rec);

            Harriet.Logging.Log
              ("surfaces",
               Rec.Name & ": done");

         end;
      end if;

      declare
         World_Rec : constant Harriet.Db.World.World_Type :=
                       Harriet.Db.World.Get (World);
         System    : constant Harriet.Db.Star_System.Star_System_Type :=
                       Harriet.Db.Star_System.Get
                         (World_Rec.Star_System);
      begin
         Harriet.Configure.Resources.Create_Deposits
           (World     => World_Rec,
            Generator =>
              Harriet.Configure.Resources.Create_Generator
                (System.X, System.Y, System.Z));
      end;

   end Generate_Surface;

   ---------------------
   -- Get_Frequencies --
   ---------------------

   function Get_Frequencies
     (World : Harriet.Db.World.World_Type)
      return Heights.Frequency_Map
   is
      type Climate_Terrain_Record is
         record
            Terrain  : Harriet.Db.Terrain_Reference;
            Min, Max : Integer;
            Count    : Natural;
         end record;

      package Climate_Terrain_Vectors is
        new Ada.Containers.Vectors (Positive, Climate_Terrain_Record);

      Vector : Climate_Terrain_Vectors.Vector;
      Total  : Natural := 0;

      Freq : Heights.Frequency_Map (1 .. Elevation.Last_Index) :=
        (others => 0);

      procedure Interpolate
        (Region : in out Heights.Frequency_Map;
         Next   : Natural);

      -----------------
      -- Interpolate --
      -----------------

      procedure Interpolate
        (Region : in out Heights.Frequency_Map;
         Next   : Natural)
      is
         pragma Unreferenced (Next);
         Start  : constant Non_Negative_Real := Real (Region (Region'First));
--           Finish : constant Non_Negative_Real := Real (Next);
         Mean   : constant Non_Negative_Real := Real (Start);
         Length : constant Non_Negative_Real :=
           Real (Region'Length);
--           Line   : array (Region'Range) of Non_Negative_Real :=
--             (others => Mean);
      begin

--           for I in Line'Range loop
--              Line (I) := Start +
--                (Finish - Start) * Real (I - Line'First)
--                / Real (Line'Last - Line'First + 1);
--              Total := Total + Line (I);
--           end loop;
--
--           Ada.Text_IO.Put_Line
--             ("interpolating: heights" & Natural'Image (Region'First)
--              & " .." & Natural'Image (Region'Last)
--              & "; start" & Natural'Image (Region (Region'First))
--              & "; finish" & Next'Image
--              & "; mean "
--              & Harriet.Real_Images.Approximate_Image (Mean)
--              & "; area" & Natural'Image (Natural (Total)));
--
--           for H of Line loop
--              H := H * Mean / Total;
--           end loop;

         for I in Region'Range loop
            Region (I) := Natural (100.0 * Mean / Length);
         end loop;

      end Interpolate;

   begin
      for Climate_Terrain of
        Harriet.Db.Climate_Terrain.Select_By_Climate
          (World.Climate)
      loop
         declare
            Terrain : constant Harriet.Db.Terrain.Terrain_Type :=
              Harriet.Db.Terrain.Get (Climate_Terrain.Terrain);
         begin
            Vector.Append (Climate_Terrain_Record'
                             (Terrain => Climate_Terrain.Terrain,
                              Min     => Terrain.Min,
                              Max     => Terrain.Max,
                              Count   => Climate_Terrain.Frequency));
            Total := Total + Climate_Terrain.Frequency;
            Freq (Terrain.Min + First_Land - 1) := Climate_Terrain.Frequency;
         end;
      end loop;

      if World.Hydrosphere > 0.0 then
         declare
            New_Total : constant Natural :=
                          Natural (1.0 / (1.0 - World.Hydrosphere)
                                   * Real (Total));
            Water_Freq : constant Natural := New_Total - Total;
         begin
            Freq (Freq'First) := Water_Freq;
         end;
      end if;

      declare
         Index       : Positive := Freq'First;
         Start_Index : Positive;
         This_F      : Natural;
      begin
         while Index <= Freq'Last
           and then Freq (Index) = 0
         loop
            Index := Index + 1;
         end loop;

         pragma Assert (Index <= Freq'Last);

         Start_Index := Index;
         This_F := Freq (Index);
         Index := Index + 1;

         while Index <= Freq'Last loop
            if Freq (Index) > 0 then
               Interpolate (Freq (Start_Index .. Index - 1), Freq (Index));
               Start_Index := Index;
               This_F := Freq (Index);
            end if;
            Index := Index + 1;
         end loop;

         if This_F > 0 then
            Interpolate (Freq (Start_Index .. Freq'Last), 0);
         end if;
      end;

      declare
         Total : Natural := 0;
         Cum   : Natural := 0;
      begin
         for I in Freq'Range loop
            Total := Total + Freq (I);
         end loop;

         for I in Freq'Range loop
            Cum := Cum + Freq (I);
         end loop;
      end;
      return Freq;

   end Get_Frequencies;

   ------------------
   -- Save_Surface --
   ------------------

   procedure Save_Surface
     (Surface   : Harriet.Surfaces.Surface_Type;
      World     : Harriet.Db.World.World_Type)
   is
      Freqs : constant Heights.Frequency_Map :=
                Get_Frequencies (World);
      Hs    : Heights.Height_Array (1 .. Natural (Surface.Tile_Count));

      Tile_Refs : array (1 .. Surface.Tile_Count)
        of Harriet.Db.Sector_Reference;

      function Base_Temperature
        (Tile : Surfaces.Surface_Tile_Index)
         return Non_Negative_Real;

      function Get_Neighbours
        (Index : Positive)
         return Heights.Neighbour_Array;

      function Base_Temperature
        (Tile : Surfaces.Surface_Tile_Index)
         return Non_Negative_Real
      is
         Y : constant Real := Surface.Tile_Centre (Tile) (2);
      begin
         return World.Average_Temperature
           + (0.5 - abs Y) * 10.0;
      end Base_Temperature;

      --------------------
      -- Get_Neighbours --
      --------------------

      function Get_Neighbours
        (Index : Positive)
         return Heights.Neighbour_Array
      is
         Tile : constant Surfaces.Surface_Tile_Index :=
                  Surfaces.Surface_Tile_Index (Index);
      begin
         return Ns : Heights.Neighbour_Array
           (1 .. Natural (Surface.Neighbour_Count (Tile)))
         do
            for I in Ns'Range loop
               Ns (I) :=
                 Positive
                   (Surface.Neighbour
                      (Tile,
                       Surfaces.Tile_Neighbour_Index (I)));
            end loop;
         end return;
      end Get_Neighbours;

   begin

      Heights.Generate_Height_Map
        (Heights     => Hs,
         Frequencies => Freqs,
         Smoothing   => 3,
         Neighbours  => Get_Neighbours'Access);

      for I in Tile_Refs'Range loop
         declare
            Centre : constant Harriet.Surfaces.Vector_3 :=
              Surface.Tile_Centre (I);
            E      : constant Harriet.Db.Elevation.Elevation_Type :=
              Harriet.Db.Elevation.Get
                (Elevation.Element (Hs (Positive (I))));
            Sector : constant Harriet.Db.World_Sector_Reference :=
                       Harriet.Db.World_Sector.Create
                         (Surface             => World.Get_Surface_Reference,
                          X                   => Centre (1),
                          Y                   => Centre (2),
                          Z                   => Centre (3),
                          Faction             =>
                            Harriet.Db.Null_Faction_Reference,
                          World               => World.Get_World_Reference,
                          Terrain             => E.Terrain,
                          Height              => E.Height,
                          Elevation           => E.Get_Elevation_Reference,
                          Sector_Use          =>
                            Harriet.Db.Null_Sector_Use_Reference,
                          Average_Temperature => Base_Temperature (I));
            S      : constant Harriet.Db.Sector_Reference :=
                       Harriet.Db.World_Sector.Get (Sector)
                       .Get_Sector_Reference;
         begin
            Tile_Refs (I) := S;
            for V of Surface.Tile_Boundary (I) loop
               Harriet.Db.Sector_Vertex.Create
                 (Sector => S,
                  X      => V (1),
                  Y      => V (2),
                  Z      => V (3));
            end loop;
         end;
      end loop;

      for Tile_Index in 1 .. Surface.Tile_Count loop
         for I in 1 .. Surface.Neighbour_Count (Tile_Index) loop
            Harriet.Db.Sector_Neighbour.Create
              (Sector    => Tile_Refs (Tile_Index),
               Neighbour => Tile_Refs (Surface.Neighbour (Tile_Index, I)));
         end loop;
      end loop;

   end Save_Surface;

end Harriet.Configure.Worlds;
