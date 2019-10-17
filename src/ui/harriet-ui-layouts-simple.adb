package body Harriet.UI.Layouts.Simple is

   type Simple_Layout_Algorithm is
     new Layout_Algorithm_Interface with null record;

   overriding function New_Tile
     (Algorithm : Simple_Layout_Algorithm;
      Layout    : in out Layout_Type'Class)
      return Tile_Index;

   overriding procedure Delete_Tile
     (Algorithm : Simple_Layout_Algorithm;
      Layout    : in out Layout_Type'Class;
      Index     : Tile_Index);

   Singleton : aliased Simple_Layout_Algorithm;

   generic
      type Count_Type is range <>;
      type Vector (<>) is private;
      with function Last_Index (V : Vector) return Count_Type;
      with function Get_Length
        (V     : Vector;
         Index : Count_Type) return Unit_Real;
      with procedure Set_Length
        (V      : in out Vector;
         Index  : Count_Type;
         Length : Unit_Real);
   procedure Update_Vector
     (V : in out Vector);

   procedure Update_Sizes
     (Layout : in out Layout_Type'Class);

   -----------------
   -- Delete_Tile --
   -----------------

   overriding procedure Delete_Tile
     (Algorithm : Simple_Layout_Algorithm;
      Layout    : in out Layout_Type'Class;
      Index     : Tile_Index)
   is
      pragma Unreferenced (Algorithm);
      Col : Layout_Column_Index;
      Row : Layout_Row_Index;
   begin
      Layout.Find_Tile (Index, Col, Row);
      if Col = 1 then
         if Layout.Column_Count = 1 then
            Layout.Delete_Column (1);
         else
            Layout.Set_Tile (1, 1, Layout.Get_Tile (2, 1));
            Layout.Delete_Row (2, 1);
            if Layout.Row_Count (2) = 0 then
               Layout.Delete_Column (2);
            end if;
         end if;
      else
         Layout.Delete_Row (Col, Row);
         if Layout.Row_Count (Col) = 0 then
            Layout.Delete_Column (Col);
         end if;
      end if;
      Update_Sizes (Layout);
   end Delete_Tile;

   --------------
   -- New_Tile --
   --------------

   overriding function New_Tile
     (Algorithm : Simple_Layout_Algorithm;
      Layout    : in out Layout_Type'Class)
      return Tile_Index
   is
      pragma Unreferenced (Algorithm);
      Column_Index : Layout_Column_Index;
   begin
      if Layout.Column_Count = 0 then
         Column_Index := Layout.New_Column;
         Layout.Set_Column_Width (Column_Index, 1.0);
      elsif Layout.Column_Count = 1 then
         Column_Index := Layout.New_Column;
         Layout.Set_Column_Width (1, 0.5);
         Layout.Set_Column_Width (2, 0.5);
      else
         Column_Index := 2;
      end if;

      return Tile : constant Tile_Index :=
        Layout.Append_Tile (Column_Index)
      do
         Update_Sizes (Layout);
      end return;
   end New_Tile;

   -------------------
   -- Simple_Layout --
   -------------------

   function Simple_Layout
      return Layout_Algorithm
   is
   begin
      return Singleton'Access;
   end Simple_Layout;

   ------------------
   -- Update_Sizes --
   ------------------

   procedure Update_Sizes
     (Layout : in out Layout_Type'Class)
   is
      procedure Update_Columns is
        new Update_Vector
          (Count_Type => Layout_Column_Count,
           Vector     => Layout_Type'Class,
           Last_Index => Column_Count,
           Get_Length => Column_Width,
           Set_Length => Set_Column_Width);

      procedure Update_Rows (Column : Layout_Column_Index);

      -----------------
      -- Update_Rows --
      -----------------

      procedure Update_Rows (Column : Layout_Column_Index) is

         Row_Count : constant Layout_Row_Count :=
                       Layout.Row_Count (Column);

         function Row_Height (Row : Layout_Row_Index) return Unit_Real
         is (Layout.Row_Height (Column, Row));

         procedure Set_Row_Height
           (Row    : Layout_Row_Index;
            Height : Unit_Real);

         --------------------
         -- Set_Row_Height --
         --------------------

         procedure Set_Row_Height
           (Row    : Layout_Row_Index;
            Height : Unit_Real)
         is
         begin
            Layout.Set_Row_Height (Column, Row, Height);
         end Set_Row_Height;

         Total_Length  : Non_Negative_Real := 0.0;
         Missing_Length : Layout_Row_Count := 0;
      begin
         if Row_Count = 0 then
            return;
         end if;

         if Row_Count = 1 then
            Set_Row_Height (1, 1.0);
         else
            for I in 1 .. Row_Count loop
               declare
                  Length : constant Unit_Real := Row_Height (I);
               begin
                  Total_Length := Total_Length + Length;
                  if Length = 0.0 then
                     Missing_Length := I;
                  end if;
               end;
            end loop;

            if Total_Length > 0.0
              and then Total_Length /= 1.0
            then
               for I in 1 .. Row_Count loop
                  Set_Row_Height (I, Row_Height (I) / Total_Length);
               end loop;
               Total_Length := 1.0;
            end if;

            if Missing_Length > 0 then
               if Total_Length < 1.0 then
                  Set_Row_Height (Missing_Length, 1.0 - Total_Length);
               else
                  declare
                     New_Length : constant Unit_Real :=
                                    1.0 / Non_Negative_Real (Row_Count);
                  begin
                     for I in 1 .. Row_Count loop
                        Set_Row_Height (I, New_Length);
                     end loop;
                  end;
               end if;
            end if;
         end if;
      end Update_Rows;

   begin
      Update_Columns (Layout);

      for I in 1 .. Layout.Column_Count loop
         Update_Rows (I);
      end loop;
   end Update_Sizes;

   -------------------
   -- Update_Vector --
   -------------------

   procedure Update_Vector
     (V : in out Vector)
   is
      Total_Length  : Non_Negative_Real := 0.0;
      Missing_Length : Count_Type := 0;
   begin
      if Last_Index (V) = 0 then
         return;
      end if;
      if Last_Index (V) = 1 then
         Set_Length (V, 1, 1.0);
      else
         for I in 1 .. Last_Index (V) loop
            declare
               Length : constant Unit_Real := Get_Length (V, I);
            begin
               Total_Length := Total_Length + Length;
               if Length = 0.0 then
                  Missing_Length := I;
               end if;
            end;
         end loop;

         if Total_Length > 1.0 then
            for I in 1 .. Last_Index (V) loop
               Set_Length (V, I, Get_Length (V, I) / Total_Length);
            end loop;
            Total_Length := 1.0;
         end if;

         if Missing_Length > 0 then
            if Total_Length < 1.0 then
               Set_Length (V, Missing_Length, 1.0 - Total_Length);
            else
               declare
                  New_Length : constant Unit_Real :=
                                 1.0 / Non_Negative_Real (Last_Index (V));
               begin
                  for I in 1 .. Last_Index (V) loop
                     Set_Length (V, I, New_Length);
                  end loop;
               end;
            end if;
         end if;
      end if;
   end Update_Vector;

end Harriet.UI.Layouts.Simple;
