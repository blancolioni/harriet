private with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Json;

package Harriet.Outliner is

   type Harriet_Outliner is tagged private;

   type Outliner_Item is private;

   function New_Item
     (Id    : String;
      Label : String)
      return Outliner_Item;

   procedure Append
     (Parent : Outliner_Item;
      Child  : Outliner_Item);

   procedure Append
     (Parent : in out Harriet_Outliner;
      Child  : Outliner_Item);

   function Serialize
     (Outliner : Harriet_Outliner)
      return Json.Json_Value'Class;

private

   type Outliner_Record;

   type Outliner_Item is access Outliner_Record;

   package Outliner_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Outliner_Item);

   type Harriet_Outliner is tagged
      record
         Top : Outliner_Lists.List;
      end record;

end Harriet.Outliner;
