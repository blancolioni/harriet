with Ada.Text_IO;

package body Harriet.Writers is

   type Null_Writer_Record is
     new Writer_Interface with null record;

   overriding procedure Put
     (Writer : in out Null_Writer_Record;
      Text   : String)
   is null;

   overriding procedure New_Line
     (Writer : in out Null_Writer_Record)
   is null;

   overriding procedure Put_Error
     (Writer : in out Null_Writer_Record;
      Text   : String);

   ---------
   -- Add --
   ---------

   procedure Add
     (To         : in out Identifier_List;
      Identifier : String)
   is
   begin
      To.List.Append (Identifier);
   end Add;

   -----------------
   -- Null_Writer --
   -----------------

   function Null_Writer return Writer_Interface'Class is
   begin
      return Writer : Null_Writer_Record;
   end Null_Writer;

   ---------------
   -- Put_Error --
   ---------------

   overriding procedure Put_Error
     (Writer : in out Null_Writer_Record;
      Text   : String)
   is
      pragma Unreferenced (Writer);
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "ERROR: " & Text);
   end Put_Error;

   -------------------------
   -- Put_Identifier_List --
   -------------------------

   procedure Put_Identifier_List
     (Writer : in out Writer_Interface'Class;
      List   : Identifier_List)
   is
      package Sorting is
        new String_Lists.Generic_Sorting ("<");

      Ids          : String_Lists.List := List.List;
      Longest      : Natural := 0;
      Cols         : Positive := 1;
      Col_Index    : Positive;
      Field_Width  : Positive;
      Screen_Width : constant := 72;

   begin
      Sorting.Sort (Ids);
      for Id of Ids loop
         if Id'Length > Longest then
            Longest := Id'Length;
         end if;
      end loop;

      if Longest = 0 then
         return;
      end if;

      Cols := Natural'Max (Natural'Min (Screen_Width / (Longest + 2), 6), 1);
      Field_Width := Screen_Width / Cols;

      Col_Index := 1;

      for Id of Ids loop
         declare
            Field : String (1 .. Field_Width) := (others => ' ');
         begin
            Field (1 .. Id'Length) := Id;
            Writer.Put (Field);
         end;
         if Col_Index = Cols then
            Col_Index := 1;
            Writer.New_Line;
         else
            Col_Index := Col_Index + 1;
         end if;
      end loop;

      if Col_Index /= 1 then
         Writer.New_Line;
      end if;
   end Put_Identifier_List;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Writer : in out Writer_Interface'Class;
      Text   : String)
   is
   begin
      Writer.Put (Text);
      Writer.New_Line;
   end Put_Line;

end Harriet.Writers;
