private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Harriet.Json;

package Harriet.Writers is

   type Writer_Interface is limited interface;

   procedure Put
     (Writer : in out Writer_Interface;
      Text   : String)
   is abstract;

   procedure New_Line
     (Writer : in out Writer_Interface)
   is abstract;

   procedure Put_Error
     (Writer  : in out Writer_Interface;
      Message : String)
   is abstract;

   procedure Put_Line
     (Writer : in out Writer_Interface'Class;
      Text   : String);

   procedure Control
     (Writer : in out Writer_Interface;
      Packet : Harriet.Json.Json_Value'Class)
   is null;

   procedure Return_Value
     (Writer : in out Writer_Interface;
      Value  : Harriet.Json.Json_Value'Class)
   is null;

   type Identifier_List is private;

   procedure Put_Identifier_List
     (Writer : in out Harriet.Writers.Writer_Interface'Class;
      List   : Identifier_List);

   procedure Add
     (To         : in out Identifier_List;
      Identifier : String);

   function Null_Writer return Writer_Interface'Class;

private

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Identifier_List is
      record
         List : String_Lists.List;
      end record;

end Harriet.Writers;
