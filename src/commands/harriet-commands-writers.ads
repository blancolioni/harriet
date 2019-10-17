private with Ada.Strings.Unbounded;

with Harriet.Json;
with Harriet.Writers;

package Harriet.Commands.Writers is

   type String_Writer is
     new Harriet.Writers.Writer_Interface with private;

   overriding procedure Put
     (Writer : in out String_Writer;
      Text   : String);

   overriding procedure New_Line
     (Writer : in out String_Writer);

   overriding procedure Put_Error
     (Writer  : in out String_Writer;
      Message : String);

   function To_String (Writer : String_Writer) return String;

   type Json_Writer is
     new Harriet.Writers.Writer_Interface with private;

   overriding procedure Put
     (Writer : in out Json_Writer;
      Text   : String);

   overriding procedure New_Line
     (Writer : in out Json_Writer);

   overriding procedure Put_Error
     (Writer  : in out Json_Writer;
      Message : String);

   overriding procedure Control
     (Writer : in out Json_Writer;
      Packet : Harriet.Json.Json_Value'Class);

   overriding procedure Return_Value
     (Writer : in out Json_Writer;
      Value  : Harriet.Json.Json_Value'Class);

   function To_Json
     (Writer : Json_Writer)
      return Harriet.Json.Json_Value'Class;

private

   type String_Writer is
     new Harriet.Writers.Writer_Interface with
      record
         Target : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Json_Writer is
     new Harriet.Writers.Writer_Interface with
      record
         Output_Lines     : Harriet.Json.Json_Array;
         Error_Lines      : Harriet.Json.Json_Array;
         Current_Output   : Ada.Strings.Unbounded.Unbounded_String;
         Control          : Harriet.Json.Json_Array;
         Result           : Harriet.Json.Json_Object;
      end record;

end Harriet.Commands.Writers;
