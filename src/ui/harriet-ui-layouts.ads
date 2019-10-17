private with Ada.Containers.Vectors;

package Harriet.UI.Layouts is

   type Layout_Tile is
      record
         X, Y          : Unit_Real := 0.0;
         Width, Height : Unit_Real := 0.0;
      end record;

   type Layout_Direction is (North, East, South, West);

   type Tile_Count is new Natural;
   No_Tile : constant Tile_Count := 0;

   subtype Tile_Index is Tile_Count range 1 .. Tile_Count'Last;

   type Layout_Column_Count is new Natural;
   No_Layout_Column : constant Layout_Column_Count := 0;

   subtype Layout_Column_Index is
     Layout_Column_Count range 1 .. Layout_Column_Count'Last;

   type Layout_Row_Count is new Natural;
   No_Layout_Row : constant Layout_Row_Count := 0;

   subtype Layout_Row_Index is
     Layout_Row_Count range 1 .. Layout_Row_Count'Last;

   type Layout_Type is tagged private;

   function Column_Count
     (Layout : Layout_Type'Class)
      return Layout_Column_Count;

   function Column_Width
     (Layout : Layout_Type'Class;
      Index  : Layout_Column_Index)
      return Unit_Real
     with Pre => Index <= Layout.Column_Count;

   procedure Set_Column_Width
     (Layout : in out Layout_Type'Class;
      Index  : Layout_Column_Index;
      Width  : Unit_Real)
     with Pre => Index <= Layout.Column_Count;

   function New_Column
     (Layout : in out Layout_Type'Class)
      return Layout_Column_Index;

   procedure Delete_Column
     (Layout : in out Layout_Type'Class;
      Index  : Layout_Column_Index)
     with Pre => Index <= Layout.Column_Count;

   function Row_Count
     (Layout       : Layout_Type'Class;
      Column_Index : Layout_Column_Index)
      return Layout_Row_Count
     with Pre => Column_Index <= Layout.Column_Count;

   function Row_Height
     (Layout       : Layout_Type'Class;
      Column_Index : Layout_Column_Index;
      Row_Index    : Layout_Row_Index)
      return Unit_Real
     with Pre => Column_Index <= Layout.Column_Count
     and then Row_Index <= Layout.Row_Count (Column_Index);

   procedure Set_Row_Height
     (Layout       : in out Layout_Type'Class;
      Column_Index : Layout_Column_Index;
      Row_Index    : Layout_Row_Index;
      Height       : Unit_Real)
     with Pre => Column_Index <= Layout.Column_Count
     and then Row_Index <= Layout.Row_Count (Column_Index);

   procedure Delete_Row
     (Layout       : in out Layout_Type'Class;
      Column_Index : Layout_Column_Index;
      Row_Index    : Layout_Row_Index)
     with Pre => Column_Index <= Layout.Column_Count
       and then Row_Index <= Layout.Row_Count (Column_Index);

   function Get_Tile
     (Layout       : Layout_Type'Class;
      Column_Index : Layout_Column_Index;
      Row_Index    : Layout_Row_Index)
      return Tile_Index
     with Pre => Column_Index <= Layout.Column_Count
     and then Row_Index <= Layout.Row_Count (Column_Index);

   procedure Set_Tile
     (Layout       : in out Layout_Type'Class;
      Column_Index : Layout_Column_Index;
      Row_Index    : Layout_Row_Index;
      Tile         : Tile_Index)
     with Pre => Column_Index <= Layout.Column_Count
     and then Row_Index <= Layout.Row_Count (Column_Index);

   function New_Tile
     (Layout       : in out Layout_Type'Class;
      Column_Index : Layout_Column_Index;
      Before_Row   : Layout_Row_Index)
      return Tile_Index;

   function Append_Tile
     (Layout       : in out Layout_Type'Class;
      Column_Index : Layout_Column_Index)
      return Tile_Index
   is (Layout.New_Tile (Column_Index, Layout.Row_Count (Column_Index) + 1));

   procedure Find_Tile
     (Layout : Layout_Type'Class;
      Tile   : Tile_Index;
      Column : out Layout_Column_Count;
      Row    : out Layout_Row_Count);

   function Get_Neighbour_Tile
     (Layout    : Layout_Type'Class;
      Tile      : Tile_Index;
      Direction : Layout_Direction)
      return Tile_Count;

   procedure Swap_Tiles
     (Layout : in out Layout_Type'Class;
      Tile_1 : Tile_Index;
      Tile_2 : Tile_Index);

   procedure Iterate
     (Layout  : Layout_Type'Class;
      Process : not null access
        procedure (Index : Tile_Index;
                   Tile  : Layout_Tile));

   type Layout_Algorithm_Interface is interface;

   function New_Tile
     (Algorithm : Layout_Algorithm_Interface;
      Layout    : in out Layout_Type'Class)
     return Tile_Index
   is abstract;

   procedure Delete_Tile
     (Algorithm : Layout_Algorithm_Interface;
      Layout    : in out Layout_Type'Class;
      Index     : Tile_Index)
   is abstract;

   type Layout_Algorithm is access constant Layout_Algorithm_Interface'Class;

   function Default_Layout return Layout_Algorithm;

private

   type Tile_Record is
      record
         Index  : Tile_Index;
         Height : Unit_Real  := 0.0;
      end record;

   package Row_Vectors is
     new Ada.Containers.Vectors (Layout_Row_Index, Tile_Record);

   type Column_Record is
      record
         Width : Unit_Real;
         Rows  : Row_Vectors.Vector;
      end record;

   package Column_Vectors is
     new Ada.Containers.Vectors
       (Layout_Column_Index, Column_Record);

   type Layout_Type is tagged
      record
         Columns   : Column_Vectors.Vector;
         Next_Tile : Tile_Index := 1;
      end record;

end Harriet.UI.Layouts;
