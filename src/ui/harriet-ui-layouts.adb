with Harriet.UI.Layouts.Simple;

package body Harriet.UI.Layouts is

   ------------------
   -- Column_Count --
   ------------------

   function Column_Count
     (Layout : Layout_Type'Class)
      return Layout_Column_Count
   is
   begin
      return Layout.Columns.Last_Index;
   end Column_Count;

   ------------------
   -- Column_Width --
   ------------------

   function Column_Width
     (Layout : Layout_Type'Class;
      Index  : Layout_Column_Index)
      return Unit_Real
   is
   begin
      return Layout.Columns.Element (Index).Width;
   end Column_Width;

   --------------------
   -- Default_Layout --
   --------------------

   function Default_Layout return Layout_Algorithm is
   begin
      return Simple.Simple_Layout;
   end Default_Layout;

   -------------------
   -- Delete_Column --
   -------------------

   procedure Delete_Column
     (Layout : in out Layout_Type'Class;
      Index  : Layout_Column_Index)
   is
   begin
      Layout.Columns.Delete (Index);
   end Delete_Column;

   ----------------
   -- Delete_Row --
   ----------------

   procedure Delete_Row
     (Layout       : in out Layout_Type'Class;
      Column_Index : Layout_Column_Index;
      Row_Index    : Layout_Row_Index)
   is
   begin
      Layout.Columns (Column_Index).Rows.Delete (Row_Index);
   end Delete_Row;

   ---------------
   -- Find_Tile --
   ---------------

   procedure Find_Tile
     (Layout : Layout_Type'Class;
      Tile   : Tile_Index;
      Column : out Layout_Column_Count;
      Row    : out Layout_Row_Count)
   is
   begin
      for C in 1 .. Layout.Column_Count loop
         for R in 1 .. Layout.Row_Count (C) loop
            if Layout.Get_Tile (C, R) = Tile then
               Column := C;
               Row := R;
               return;
            end if;
         end loop;
      end loop;
      Column := 0;
      Row := 0;
   end Find_Tile;

   ------------------------
   -- Get_Neighbour_Tile --
   ------------------------

   function Get_Neighbour_Tile
     (Layout    : Layout_Type'Class;
      Tile      : Tile_Index;
      Direction : Layout_Direction)
      return Tile_Count
   is
      Column : Layout_Column_Count;
      Row    : Layout_Row_Count;
   begin
      Layout.Find_Tile (Tile, Column, Row);
      if Column = 0 or else Row = 0 then
         return 0;
      end if;

      case Direction is
         when North =>
            if Row = 1 then
               return 0;
            end if;
            Row := Row - 1;
         when South =>
            if Row = Layout.Row_Count (Column) then
               return 0;
            end if;
            Row := Row + 1;
         when West =>
            if Column = 1
              or else Layout.Row_Count (Column - 1) = 0
            then
               return 0;
            end if;

            Column := Column - 1;
            Row := 1;
         when East =>
            if Column = Layout.Column_Count
              or else Layout.Row_Count (Column + 1) = 0
            then
               return 0;
            end if;
            Column := Column + 1;
            Row := 1;
      end case;

      return Layout.Get_Tile (Column, Row);
   end Get_Neighbour_Tile;

   --------------
   -- Get_Tile --
   --------------

   function Get_Tile
     (Layout       : Layout_Type'Class;
      Column_Index : Layout_Column_Index;
      Row_Index    : Layout_Row_Index)
      return Tile_Index
   is
   begin
      return Layout.Columns (Column_Index).Rows (Row_Index).Index;
   end Get_Tile;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Layout  : Layout_Type'Class;
      Process : not null access
        procedure (Index : Tile_Index;
                   Tile  : Layout_Tile))
   is
      Left : Unit_Real := 0.0;
   begin
      for Col of Layout.Columns loop
         declare
            Top : Unit_Real := 0.0;
         begin
            for Row of Col.Rows loop
               Process (Row.Index, (Left, Top, Col.Width, Row.Height));
               Top := Top + Row.Height;
            end loop;
         end;
         Left := Left + Col.Width;
      end loop;
   end Iterate;

   ----------------
   -- New_Column --
   ----------------

   function New_Column
     (Layout : in out Layout_Type'Class)
      return Layout_Column_Index
   is
   begin
      Layout.Columns.Append ((others => <>));
      return Layout.Columns.Last_Index;
   end New_Column;

   -------------
   -- New_Row --
   -------------

   function New_Tile
     (Layout       : in out Layout_Type'Class;
      Column_Index : Layout_Column_Index;
      Before_Row   : Layout_Row_Index)
      return Tile_Index
   is
      Rows : Row_Vectors.Vector renames Layout.Columns (Column_Index).Rows;
   begin
      Rows.Insert (Before_Row, (Layout.Next_Tile, 0.0));
      Layout.Next_Tile := Layout.Next_Tile + 1;
      return Layout.Next_Tile - 1;
   end New_Tile;

   ---------------
   -- Row_Count --
   ---------------

   function Row_Count
     (Layout       : Layout_Type'Class;
      Column_Index : Layout_Column_Index)
      return Layout_Row_Count
   is
   begin
      return Layout.Columns (Column_Index).Rows.Last_Index;
   end Row_Count;

   ----------------
   -- Row_Height --
   ----------------

   function Row_Height
     (Layout       : Layout_Type'Class;
      Column_Index : Layout_Column_Index;
      Row_Index    : Layout_Row_Index)
      return Unit_Real
   is
   begin
      return Layout.Columns (Column_Index).Rows (Row_Index).Height;
   end Row_Height;

   ----------------------
   -- Set_Column_Width --
   ----------------------

   procedure Set_Column_Width
     (Layout : in out Layout_Type'Class;
      Index  : Layout_Column_Index;
      Width  : Unit_Real)
   is
   begin
      Layout.Columns (Index).Width := Width;
   end Set_Column_Width;

   --------------------
   -- Set_Row_Height --
   --------------------

   procedure Set_Row_Height
     (Layout       : in out Layout_Type'Class;
      Column_Index : Layout_Column_Index;
      Row_Index    : Layout_Row_Index;
      Height       : Unit_Real)
   is
   begin
      Layout.Columns (Column_Index).Rows (Row_Index).Height := Height;
   end Set_Row_Height;

   --------------
   -- Set_Tile --
   --------------

   procedure Set_Tile
     (Layout       : in out Layout_Type'Class;
      Column_Index : Layout_Column_Index;
      Row_Index    : Layout_Row_Index;
      Tile         : Tile_Index)
   is
   begin
      Layout.Columns (Column_Index).Rows (Row_Index).Index := Tile;
   end Set_Tile;

   ----------------
   -- Swap_Tiles --
   ----------------

   procedure Swap_Tiles
     (Layout : in out Layout_Type'Class;
      Tile_1 : Tile_Index;
      Tile_2 : Tile_Index)
   is
      Column_1, Column_2 : Layout_Column_Index;
      Row_1, Row_2       : Layout_Row_Index;
   begin
      Layout.Find_Tile (Tile_1, Column_1, Row_1);
      Layout.Find_Tile (Tile_2, Column_2, Row_2);
      Layout.Set_Tile (Column_1, Row_1, Tile_2);
      Layout.Set_Tile (Column_2, Row_2, Tile_1);
   end Swap_Tiles;

end Harriet.UI.Layouts;
