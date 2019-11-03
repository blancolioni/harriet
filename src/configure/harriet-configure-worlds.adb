with Ada.Containers.Vectors;
with Ada.Text_IO;

with WL.Random.Height_Maps;

with Harriet.Solar_System;
with Harriet.Surfaces;

with Harriet.Configure.Resources;

with Harriet.Db.Climate_Terrain;
with Harriet.Db.Sector_Neighbour;
with Harriet.Db.Sector_Vertex;
with Harriet.Db.Star_System;
with Harriet.Db.Terrain;
with Harriet.Db.World;
with Harriet.Db.World_Sector;

package body Harriet.Configure.Worlds is

   package Heights renames WL.Random.Height_Maps;

   package Terrain_Vectors is
     new Ada.Containers.Vectors
       (Positive, Harriet.Db.Terrain_Reference, Harriet.Db."=");

   Terrain     : Terrain_Vectors.Vector;
   Water       : Harriet.Db.Terrain_Reference :=
                   Harriet.Db.Null_Terrain_Reference;
   Water_Index : Natural := 0;

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

      if Terrain.Is_Empty then
         for T of Harriet.Db.Terrain.Scan_By_Tag loop
            Terrain.Append (T.Get_Terrain_Reference);
            if T.Is_Water then
               Water := T.Get_Terrain_Reference;
               Water_Index := Terrain.Last_Index;
            end if;
         end loop;
      end if;

      case Rec.Composition is
         when Ice | Rock | Rock_Ice | Rock_Iron =>
            Tile_Count := Natural (Radius * 400.0);
         when Hydrogen | Gaseous =>
            Tile_Count := 0;
      end case;

      if Tile_Count = 0 then
         return;
      end if;

      declare
         Surface   : Harriet.Surfaces.Root_Surface_Type;
      begin
         Ada.Text_IO.Put_Line
           (Rec.Name & ": creating surface with "
            & Tile_Count'Image & " tiles");
         Surface.Create_Voronoi_Partition (Tile_Count);
         Save_Surface (Surface, Rec);

         Ada.Text_IO.Put_Line
           (Rec.Name & ": done");

      end;

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

   function Get_Frequencies
     (World : Harriet.Db.World.World_Type)
      return Heights.Frequency_Map
   is
      type Climate_Terrain_Record is
         record
            Terrain : Harriet.Db.Terrain_Reference;
            Index   : Positive;
            Count   : Natural;
         end record;

      package Climate_Terrain_Vectors is
        new Ada.Containers.Vectors (Positive, Climate_Terrain_Record);

      Vector : Climate_Terrain_Vectors.Vector;
      Total : Natural := 0;

   begin
      for Climate_Terrain of
        Harriet.Db.Climate_Terrain.Select_By_Climate
          (World.Climate)
      loop
         declare
            use type Harriet.Db.Terrain_Reference;
            Index : Natural := 0;
         begin
            for I in 1 .. Terrain.Last_Index loop
               if Terrain.Element (I) = Climate_Terrain.Terrain then
                  Index := I;
                  exit;
               end if;
            end loop;

            if Index = 0 then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "bad terrain:"
                  & Harriet.Db.To_String (Climate_Terrain.Terrain));
            else
               Vector.Append (Climate_Terrain_Record'
                                (Terrain => Climate_Terrain.Terrain,
                                 Index   => Index,
                                 Count   => Climate_Terrain.Frequency));
               Total := Total + Climate_Terrain.Frequency;
            end if;
         end;
      end loop;

      if World.Hydrosphere > 0.0 then
         declare
            New_Total : constant Natural :=
                          Natural (1.0 / (1.0 - World.Hydrosphere)
                                   * Real (Total));
            Water_Freq : constant Natural := New_Total - Total;
         begin
            Vector.Append (Climate_Terrain_Record'
                             (Terrain => Water,
                              Index   => Water_Index,
                              Count   => Water_Freq));
         end;
      end if;

      return Freqs : Heights.Frequency_Map (1 .. Terrain.Last_Index) :=
        (others => 0)
      do
         for Item of Vector loop
            Freqs (Item.Index) := Item.Count;
         end loop;
      end return;

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
         Smoothing   => 4,
         Neighbours  => Get_Neighbours'Access);

      for I in Tile_Refs'Range loop
         declare
            Centre : constant Harriet.Surfaces.Vector_3 :=
                       Surface.Tile_Centre (I);
            Sector : constant Harriet.Db.World_Sector_Reference :=
                       Harriet.Db.World_Sector.Create
                         (Surface             => World.Get_Surface_Reference,
                          X                   => Centre (1),
                          Y                   => Centre (2),
                          Z                   => Centre (3),
                          World               => World.Get_World_Reference,
                          Terrain             =>
                            Terrain.Element (Hs (Positive (I))),
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
