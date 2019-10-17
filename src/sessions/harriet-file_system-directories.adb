with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Strings.Unbounded;

with WL.String_Maps;

package body Harriet.File_System.Directories is

   package Node_Id_Holders is
     new Ada.Containers.Indefinite_Holders (Node_Id);

   type Child_Node_Record is
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
         Node : Node_Id_Holders.Holder;
      end record;

   package Child_Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Child_Node_Record);

   package Child_Node_Maps is
     new WL.String_Maps (Node_Id);

   type Directory_Record is
     new Branch_Node with
      record
         Child_List : Child_Node_Lists.List;
         Child_Map  : Child_Node_Maps.Map;
      end record;

   overriding function Has_Child
     (Node : Directory_Record;
      Name : String)
      return Boolean
   is (Node.Child_Map.Contains (Name));

   overriding function Get_Child
     (Node  : Directory_Record;
      Child : String)
      return Node_Id
   is (Node.Child_Map.Element (Child));

   overriding procedure Iterate_Children
     (Node    : Directory_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id));

   overriding procedure Bind_Child
     (Node  : in out Directory_Record;
      Name  : String;
      Child : Node_Id);

   overriding procedure Delete_Child
     (Node   : in out Directory_Record;
      Name   : String);

   ----------------
   -- Bind_Child --
   ----------------

   overriding procedure Bind_Child
     (Node  : in out Directory_Record;
      Name  : String;
      Child : Node_Id)
   is
   begin
      Node.Child_List.Append
        (Child_Node_Record'
           (Name => Ada.Strings.Unbounded.To_Unbounded_String (Name),
            Node => Node_Id_Holders.To_Holder (Child)));
      Node.Child_Map.Insert
        (Name, Child);
   end Bind_Child;

   ------------------
   -- Delete_Child --
   ------------------

   overriding procedure Delete_Child
     (Node   : in out Directory_Record;
      Name   : String)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
      Position : Child_Node_Lists.Cursor := Node.Child_List.First;
   begin
      while Child_Node_Lists.Element (Position).Name /= Name loop
         Child_Node_Lists.Next (Position);
      end loop;

      Node.Child_List.Delete (Position);
      Node.Child_Map.Delete (Name);
   end Delete_Child;

   --------------------
   -- Directory_Node --
   --------------------

   function Directory_Node return Node_Interface'Class is
   begin
      return Directory_Record'
        (Child_List => <>,
         Child_Map  => <>);
   end Directory_Node;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (Node    : Directory_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id))
   is
   begin
      for Child of Node.Child_List loop
         Process (Ada.Strings.Unbounded.To_String (Child.Name),
                  Child.Node.Element);
      end loop;
   end Iterate_Children;

end Harriet.File_System.Directories;
