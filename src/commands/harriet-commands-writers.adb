package body Harriet.Commands.Writers is

   ----------
   -- Copy --
   ----------

   overriding procedure Control
     (Writer : in out Json_Writer;
      Packet : Harriet.Json.Json_Value'Class)
   is
   begin
      Writer.Control.Append (Packet);
   end Control;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line (Writer : in out String_Writer) is
      use Ada.Strings.Unbounded;
   begin
      Writer.Target := Writer.Target
        & Character'Val (10);
   end New_Line;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line (Writer : in out Json_Writer) is
      use Ada.Strings.Unbounded;
   begin
      Writer.Output_Lines.Append
        (Harriet.Json.String_Value
           (Ada.Strings.Unbounded.To_String (Writer.Current_Output)));
      Writer.Current_Output :=
        Ada.Strings.Unbounded.Null_Unbounded_String;
   end New_Line;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Writer : in out String_Writer;
      Text   : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Writer.Target := Writer.Target & Text;
   end Put;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Writer : in out Json_Writer;
      Text   : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Writer.Current_Output := Writer.Current_Output & Text;
   end Put;

   ---------------
   -- Put_Error --
   ---------------

   overriding procedure Put_Error
     (Writer  : in out String_Writer;
      Message : String)
   is
   begin
      Writer.Put_Line (Message);
   end Put_Error;

   ---------------
   -- Put_Error --
   ---------------

   overriding procedure Put_Error
     (Writer  : in out Json_Writer;
      Message : String)
   is
   begin
      Writer.Error_Lines.Append
        (Harriet.Json.String_Value (Message));
   end Put_Error;

   ------------------
   -- Return_Value --
   ------------------

   overriding procedure Return_Value
     (Writer : in out Json_Writer;
      Value  : Harriet.Json.Json_Value'Class)
   is
   begin
      Writer.Result.Set_Property ("result", Value);
   end Return_Value;

   -------------
   -- To_Json --
   -------------

   function To_Json
     (Writer : Json_Writer)
      return Harriet.Json.Json_Value'Class
   is
   begin
      return Object : Harriet.Json.Json_Object do
         Object.Set_Property
           ("standardOutput", Writer.Output_Lines);
         Object.Set_Property
           ("standardError", Writer.Error_Lines);
         Object.Set_Property
           ("control", Writer.Control);
         declare
            Value : constant Json.Json_Value'Class :=
              Writer.Result.Get_Property ("result");
         begin
            Object.Set_Property
              ("result", Value);
         end;

      end return;
   end To_Json;

   ---------------
   -- To_String --
   ---------------

   function To_String (Writer : String_Writer) return String is
   begin
      return Ada.Strings.Unbounded.To_String
        (Writer.Target);
   end To_String;

end Harriet.Commands.Writers;
