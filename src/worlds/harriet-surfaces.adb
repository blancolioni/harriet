with Ada.Containers.Doubly_Linked_Lists;
with Ada.Numerics;

with Harriet.Elementary_Functions;
with Harriet.Spheres;
with Harriet.Voronoi_Diagrams;

package body Harriet.Surfaces is

   ------------------------------
   -- Create_Voronoi_Partition --
   ------------------------------

   procedure Create_Voronoi_Partition
     (Surface : in out Root_Surface_Type'Class;
      Count   : Natural)
   is
      Pts     : Harriet.Spheres.Surface_Point_Vectors.Vector;
      Voronoi : Harriet.Voronoi_Diagrams.Voronoi_Diagram;

      procedure Create_Partition;

      ----------------------
      -- Create_Partition --
      ----------------------

      procedure Create_Partition is
         type Triangle is
            record
               Position : Vector_3;
               A, B, C : Positive;
            end record;

         package Triangle_Index_Lists is
           new Ada.Containers.Doubly_Linked_Lists (Positive);

         Vs   : array (1 .. Count) of Vector_3;
         Idxs : array (1 .. Count) of Triangle_Index_Lists.List;
         Ts : array (1 .. Voronoi.Delauny_Triangle_Count) of Triangle;
      begin

         for I in Vs'Range loop
            declare
               P : constant Harriet.Spheres.Surface_Point :=
                     Pts.Element (I);
            begin
               Vs (I) := (P.X, P.Y, P.Z);
            end;
         end loop;

         for I in Ts'Range loop
            declare
               T : Triangle renames Ts (I);
            begin
               Voronoi.Get_Delauny_Triangle
                 (I, T.A, T.B, T.C);

               for J in 1 .. 3 loop
                  T.Position (J) :=
                    (Vs (T.A) (J)
                     + Vs (T.B) (J)
                     + Vs (T.C) (J))
                      / 3.0;
               end loop;

               Idxs (T.A).Append (I);
               Idxs (T.B).Append (I);
               Idxs (T.C).Append (I);
               Surface.Vertices.Append (T.Position);
            end;
         end loop;

         for I in Idxs'Range loop
            declare

               Centre : constant Vector_3 := Vs (I);
               Normal : constant Vector_3 := Centre;

               function Cross
                 (Left, Right : Vector_3)
                  return Vector_3
                 is (Left (2) * Right (3) - Left (3) * Right (2),
                     Left (3) * Right (1) - Left (1) * Right (3),
                     Left (1) * Right (2) - Left (2) * Right (1));

               function Less_Than
                 (Index_1, Index_2 : Positive)
                  return Boolean;

               ---------------
               -- Less_Than --
               ---------------

               function Less_Than
                 (Index_1, Index_2 : Positive)
                  return Boolean
               is
                  use Real_Arrays;
                  C : constant Vector_3 :=
                        Cross (Ts (Index_1).Position - Centre,
                               Ts (Index_2).Position - Centre);
               begin
                  return Normal * C < 0.0;
               end Less_Than;

               package Triangle_Sorting is
                 new Triangle_Index_Lists.Generic_Sorting (Less_Than);
            begin
               Triangle_Sorting.Sort (Idxs (I));
            end;
         end loop;

         for I in Idxs'Range loop
            declare
               S_Pos : constant Harriet.Spheres.Surface_Point :=
                         Pts.Element (I);
               V_Pos : constant Vector_3 :=
                         (S_Pos.X, S_Pos.Y, S_Pos.Z);
               Tile : Tile_Record :=
                        Tile_Record'
                          (Position   => V_Pos,
                           Vertices   => <>,
                           Neighbours => <>);
            begin
               for V of Idxs (I) loop
                  declare
                     T : constant Triangle := Ts (V);

                     procedure Check_Neighbour (Index : Positive);

                     ---------------------
                     -- Check_Neighbour --
                     ---------------------

                     procedure Check_Neighbour (Index : Positive) is
                     begin
                        if Index = I then
                           return;
                        end if;

                        for N of Tile.Neighbours loop
                           if N = Surface_Tile_Count (Index) then
                              return;
                           end if;
                        end loop;

                        Tile.Neighbours.Append
                          (Surface_Tile_Count
                             (Index));
                     end Check_Neighbour;

                  begin
                     Tile.Vertices.Append (T.Position);
                     Check_Neighbour (T.A);
                     Check_Neighbour (T.B);
                     Check_Neighbour (T.C);
                  end;
               end loop;

               Surface.Tiles.Append (Tile);
            end;
         end loop;

      end Create_Partition;

   begin
      Harriet.Spheres.Spiral_Sphere_Points (Pts, Count);
      for Pt of Pts loop
         Voronoi.Add_Spherical_Point (Pt.X, Pt.Y, Pt.Z);
      end loop;
      Voronoi.Generate;

      Create_Partition;

   end Create_Voronoi_Partition;

   --------------
   -- Get_Tile --
   --------------

   function Get_Tile
     (Relative_Latitude  : Signed_Unit_Real;
      Relative_Longitude : Unit_Real)
      return Surface_Tile_Index
   is
      pragma Unreferenced (Relative_Latitude);
      pragma Unreferenced (Relative_Longitude);
   begin
      return 1;
   end Get_Tile;

   --------------
   -- Latitude --
   --------------

   function Latitude
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Real
   is
      use Harriet.Elementary_Functions;
   begin
      return Arcsin (Surface.Tiles.Element (Tile).Position (2),
                     Cycle => 360.0);
   end Latitude;

   ---------------
   -- Longitude --
   ---------------

   function Longitude
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Real
   is
      use Harriet.Elementary_Functions;
      V : constant Vector_3 :=
            Surface.Tiles.Element (Tile).Position;
   begin
      return Arctan (V (3), V (1), 360.0);
   end Longitude;

   -------------------
   -- Tile_Boundary --
   -------------------

   function Tile_Boundary
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Tile_Vertex_Array
   is
   begin
      return Boundary : Tile_Vertex_Array
        (1 .. Surface.Tiles.Element (Tile).Vertices.Last_Index)
      do
         for I in Boundary'Range loop
            Boundary (I) :=
              Surface.Tiles.Element (Tile).Vertices.Element (I);
         end loop;
      end return;
   end Tile_Boundary;

   -----------------
   -- Tile_Centre --
   -----------------

   function Tile_Centre
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Vector_3
   is
   begin
      return Surface.Tiles.Element (Tile).Position;
   end Tile_Centre;

end Harriet.Surfaces;
