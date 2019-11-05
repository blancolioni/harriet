with Ada.Containers.Vectors;

with WL.Random.Height_Maps;

with Harriet.Logging;
with Harriet.Solar_System;
with Harriet.Surfaces;
with Harriet.Terrain;

with Harriet.Configure.Resources;

with Harriet.Db.Climate_Terrain;
with Harriet.Db.Elevation;
with Harriet.Db.Sector_Neighbour;
with Harriet.Db.Sector_Vertex;
with Harriet.Db.Star_System;
with Harriet.Db.World;
with Harriet.Db.World_Sector;

package body Harriet.Configure.Worlds is

   package Heights renames WL.Random.Height_Maps;

   type Elevation_Terrain is
      record
         Elevation : Harriet.Db.Elevation_Reference;
         Terrain   : Harriet.Db.Terrain_Reference;
      end record;

   package Elevation_Vectors is
     new Ada.Containers.Vectors
       (Positive, Elevation_Terrain);

   type Climate_Terrain_Record is
      record
         Terrain   : Harriet.Db.Terrain_Reference;
         First     : Positive;
         Last      : Natural;
         Frequency : Unit_Real;
         Count     : Natural;
      end record;

   package Climate_Terrain_Vectors is
     new Ada.Containers.Vectors (Positive, Climate_Terrain_Record);

   procedure Save_Surface
     (Surface : Harriet.Surfaces.Surface_Type;
      World   : Harriet.Db.World.World_Type);

   procedure Get_Climate_Terrain
     (World  : Harriet.Db.World.World_Type;
      Vector : out Climate_Terrain_Vectors.Vector);

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

   -------------------------
   -- Get_Climate_Terrain --
   -------------------------

   procedure Get_Climate_Terrain
     (World  : Harriet.Db.World.World_Type;
      Vector : out Climate_Terrain_Vectors.Vector)
   is
      Water_Last : constant Natural := World.Sea_Level;
      Next       : Positive := Water_Last + 1;
   begin

      if Water_Last > 0 then
         Vector.Append
           (Climate_Terrain_Record'
              (Terrain   => Harriet.Terrain.Ocean,
               First     => 1,
               Last      => Water_Last,
               Frequency => World.Hydrosphere,
               Count     => Water_Last));
      end if;

      for Climate_Terrain of
        Harriet.Db.Climate_Terrain.Select_By_Climate
          (World.Climate)
      loop
         declare
            Frequency : constant Unit_Real :=
              Climate_Terrain.Frequency * (1.0 - World.Hydrosphere);
            Count     : constant Natural :=
              Natural (Frequency * Real (World.Elevation_Range));
         begin
            Vector.Append (Climate_Terrain_Record'
                             (Terrain   => Climate_Terrain.Terrain,
                              First     => Next,
                              Last      => Next + Count - 1,
                              Frequency => Frequency,
                              Count     => Count));
            Next := Next + Count;
         end;
      end loop;

      while Next < World.Elevation_Range loop
         declare
            Total_Move : Natural := 0;
            Remaining  : constant Natural :=
              World.Elevation_Range - Next;
         begin
            for Item of Vector loop
               declare
                  This_Move : constant Positive :=
                    Natural'Min
                      (Natural'Max (Remaining / Vector.Last_Index, 1),
                       Remaining - Total_Move);
               begin
                  Item.First := Item.First + This_Move;
                  Item.Count := Item.Count + This_Move;
                  Item.Last := Item.First + Item.Count - 1;
                  Total_Move := Total_Move + This_Move;
                  exit when Total_Move = Remaining;
               end;
            end loop;
            Next := Next + Total_Move;
         end;
      end loop;

   end Get_Climate_Terrain;

   ---------------------
   -- Get_Frequencies --
   ---------------------

   function Get_Frequencies
     (World : Harriet.Db.World.World_Type)
      return Heights.Frequency_Map
   is
   begin
      return Freq : constant Heights.Frequency_Map
        (1 .. World.Elevation_Range) := (others => 1);
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
         Y : constant Real := Surface.Tile_Centre (Tile) (3);
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

      Climate_Vector : Climate_Terrain_Vectors.Vector;
      Elevation      : Elevation_Vectors.Vector;

   begin

      WL.Random.Reset (World.Seed);

      Get_Climate_Terrain (World, Climate_Vector);

      Heights.Generate_Height_Map
        (Heights     => Hs,
         Frequencies => Freqs,
         Smoothing   => 3,
         Neighbours  => Get_Neighbours'Access);

      for E of Harriet.Db.Elevation.Scan_By_Top_Record loop
         declare
            Height  : constant Integer := E.Height + World.Sea_Level;
            Terrain : Harriet.Db.Terrain_Reference;
         begin
            if Height in 1 .. World.Elevation_Range then
               for Item of Climate_Vector loop
                  if Height in Item.First .. Item.Last then
                     Terrain := Item.Terrain;
                     exit;
                  end if;
               end loop;

               Elevation.Append
                 (Elevation_Terrain'
                    (Elevation => E.Get_Elevation_Reference,
                     Terrain   => Terrain));
            end if;
         end;
      end loop;

      for I in Tile_Refs'Range loop
         declare
            Centre : constant Harriet.Surfaces.Vector_3 :=
              Surface.Tile_Centre (I);
            E      : constant Harriet.Db.Elevation.Elevation_Type :=
              Harriet.Db.Elevation.Get
                (Elevation.Element (Hs (Positive (I))).Elevation);
            Terrain : constant Harriet.Db.Terrain_Reference :=
              Elevation.Element (Hs (Positive (I))).Terrain;
            Sector : constant Harriet.Db.World_Sector_Reference :=
                       Harriet.Db.World_Sector.Create
                         (Surface             => World.Get_Surface_Reference,
                          X                   => Centre (1),
                          Y                   => Centre (2),
                          Z                   => Centre (3),
                          Faction             =>
                            Harriet.Db.Null_Faction_Reference,
                          World               => World.Get_World_Reference,
                          Terrain             => Terrain,
                          Height              => Hs (Positive (I)),
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
