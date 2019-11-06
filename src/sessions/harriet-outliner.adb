with Ada.Strings.Unbounded;

package body Harriet.Outliner is

   use Ada.Strings.Unbounded;

   function "+" (S : String) return Unbounded_String
                 renames To_Unbounded_String;
   function "-" (S : Unbounded_String) return String
                 renames To_String;

   type Outliner_Record is
      record
         Key, Label : Unbounded_String;
         Children   : Outliner_Lists.List;
      end record;

   ------------
   -- Append --
   ------------

   procedure Append
     (Parent : Outliner_Item;
      Child  : Outliner_Item)
   is
   begin
      Parent.Children.Append (Child);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Parent : in out Harriet_Outliner; Child : Outliner_Item)
   is
   begin
      Parent.Top.Append (Child);
   end Append;

   --------------
   -- New_Item --
   --------------

   function New_Item (Id : String; Label : String) return Outliner_Item is
   begin
      return new Outliner_Record'
        (Key      => +Id,
         Label    => +Label,
         Children => <>);
   end New_Item;

   ---------------
   -- Serialize --
   ---------------

   function Serialize
     (Outliner : Harriet_Outliner)
      return Json.Json_Value'Class
   is
      procedure Add_Children
        (Node : in out Json.Json_Object;
         Children : Outliner_Lists.List);

      function Serialize_Item
        (Child : Outliner_Item)
         return Json.Json_Value'Class;

      ------------------
      -- Add_Children --
      ------------------

      procedure Add_Children
        (Node     : in out Json.Json_Object;
         Children : Outliner_Lists.List)
      is
         Arr : Json.Json_Array;
      begin
         for Child of Children loop
            Arr.Append (Serialize_Item (Child));
         end loop;
         Node.Set_Property ("nodes", Arr);
      end Add_Children;

      --------------------
      -- Serialize_Item --
      --------------------

      function Serialize_Item
        (Child : Outliner_Item)
         return Json.Json_Value'Class
      is
         Result : Json.Json_Object;
      begin
         Result.Set_Property ("key", -Child.Key);
         Result.Set_Property ("label", -Child.Label);
         Add_Children (Result, Child.Children);
         return Result;
      end Serialize_Item;

      Root : Json.Json_Object;

   begin
      Root.Set_Property ("key", "root");
      Root.Set_Property ("label", "outliner");
      Add_Children (Root, Outliner.Top);
      return Root;
   end Serialize;

end Harriet.Outliner;
