with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;

package body Harriet.File_System is

   type Root_Node_Id is new Node_Id_Interface with
      record
         Id : Natural := 0;
      end record;

   overriding function Is_Empty
     (Id : Root_Node_Id)
      return Boolean
   is (Id.Id = 0);

   overriding function Get
     (Id : Root_Node_Id)
      return Node_Interface'Class;

   overriding function Update
     (Node : Root_Node_Id)
      return access Node_Interface'Class;

   package Node_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, Node_Interface'Class);

   package Node_Sets is
     new Ada.Containers.Ordered_Sets (Positive);

   type Root_Filesystem_Record is
      record
         Node_Vector : Node_Vectors.Vector;
         Free_Set    : Node_Sets.Set;
      end record;

   Root_FS : Root_Filesystem_Record;

   type Root_Filesystem_Type is
     new File_System_Interface with null record;

   overriding function Get_Root_Node_Id
     (FS   : Root_Filesystem_Type)
      return Node_Id;

   overriding function Create
     (FS   : Root_Filesystem_Type;
      Node : Node_Interface'Class)
      return Node_Id;

   overriding procedure Delete
     (FS   : Root_Filesystem_Type;
      Node : Node_Id);

   ----------------
   -- Bind_Child --
   ----------------

   overriding procedure Bind_Child
     (Node : in out Leaf_Node;
      Name  : String;
      Child : Node_Id'Class)
   is
      pragma Unreferenced (Node, Child);
   begin
      raise Constraint_Error with
        "Bind_Child called on leaf node";
   end Bind_Child;

   --------------
   -- Contents --
   --------------

   overriding function Contents
     (Node : Branch_Node)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;

      procedure Add_Result
        (Name : String;
         Child : Node_Id);

      ----------------
      -- Add_Result --
      ----------------

      procedure Add_Result
        (Name  : String;
         Child : Node_Id)
      is
         pragma Unreferenced (Child);
      begin
         if Result /= "" then
            Result := Result & " ";
         end if;
         Result := Result & Name;
      end Add_Result;

   begin
      Branch_Node'Class (Node).Iterate_Children (Add_Result'Access);
      return To_String (Result);
   end Contents;

   ------------
   -- Create --
   ------------

   overriding function Create
     (FS   : Root_Filesystem_Type;
      Node : Node_Interface'Class)
      return Node_Id
   is
      pragma Unreferenced (FS);
      Id : Positive;
   begin
      if Root_FS.Free_Set.Is_Empty then
         Id := Root_FS.Node_Vector.Last_Index + 1;
         Root_FS.Node_Vector.Append (Node);
      else
         Id := Root_FS.Free_Set.First_Element;
         Root_FS.Free_Set.Delete_First;
         Root_FS.Node_Vector.Replace_Element (Id, Node);
      end if;
      return Root_Node_Id'(Id => Id);
   end Create;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (FS   : Root_Filesystem_Type;
      Node : Node_Id)
   is
      pragma Unreferenced (FS);
   begin
      Root_FS.Free_Set.Insert (Root_Node_Id (Node).Id);
   end Delete;

   ------------------
   -- Delete_Child --
   ------------------

   overriding procedure Delete_Child (Node : in out Leaf_Node; Name : String)
   is
      pragma Unreferenced (Node, Name);
   begin
      raise Constraint_Error with
        "Delete_Child called on leaf node";
   end Delete_Child;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Id : Root_Node_Id)
      return Node_Interface'Class
   is
      Index : constant Positive := Id.Id;
   begin
      if Root_FS.Free_Set.Contains (Index)
        or else Index > Root_FS.Node_Vector.Last_Index
      then
         raise Constraint_Error with
           "Context.File_System.Get: bad node id";
      end if;
      return Root_FS.Node_Vector.Element (Index);
   end Get;

   ---------------
   -- Get_Child --
   ---------------

   overriding function Get_Child
     (Node  : Leaf_Node;
      Child : String)
      return Node_Id
   is
      pragma Unreferenced (Node, Child);
   begin
      return (raise Constraint_Error with
                "Get_Child called on leaf node");
   end Get_Child;

   ----------------------
   -- Get_Root_Node_Id --
   ----------------------

   overriding function Get_Root_Node_Id
     (FS   : Root_Filesystem_Type)
      return Node_Id
   is
      pragma Unreferenced (FS);
   begin
      return Root_Node_Id'(Id => 1);
   end Get_Root_Node_Id;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Node : Leaf_Node; Name : String) return Boolean
   is
      pragma Unreferenced (Node, Name);
   begin
      return False;
   end Has_Child;

   -------------
   -- Is_Leaf --
   -------------

   overriding function Is_Leaf (Node : Leaf_Node) return Boolean is
      pragma Unreferenced (Node);
   begin
      return True;
   end Is_Leaf;

   -------------
   -- Is_Leaf --
   -------------

   overriding function Is_Leaf
     (Node : Branch_Node)
      return Boolean
   is
      pragma Unreferenced (Node);
   begin
      return False;
   end Is_Leaf;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (Node    : Leaf_Node;
      Process : not null access procedure
        (Name  : String;
         Child : Node_Id))
   is
      pragma Unreferenced (Node, Process);
   begin
      raise Constraint_Error with
        "Iterate_Children called on leaf node";
   end Iterate_Children;

   ---------------------
   -- Root_Filesystem --
   ---------------------

   function Root_Filesystem return File_System_Interface'Class is
   begin
      return FS : Root_Filesystem_Type;
   end Root_Filesystem;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Node : Root_Node_Id)
      return access Node_Interface'Class
   is
      Index : constant Positive := Node.Id;
   begin
      return Root_FS.Node_Vector.Reference (Index).Element;
   end Update;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Node : Read_Only_Node_Id)
      return access Node_Interface'Class
   is
      pragma Unreferenced (Node);
   begin
      return (raise Constraint_Error with
                "update: read-only filesystem");
   end Update;

end Harriet.File_System;
