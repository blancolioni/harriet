with Ada.Strings.Unbounded;

with Harriet.Db.Star_System;
with Harriet.Db.World;

package body Harriet.File_System.Universe is

   use type Harriet.Db.Star_System_Reference;

   type Star_System_Node_Id is
     new Node_Id_Interface with
      record
         Ref : Harriet.Db.Star_System_Reference;
      end record;

   overriding function Is_Empty
     (Id : Star_System_Node_Id)
      return Boolean;

   overriding function Get
     (Id : Star_System_Node_Id)
      return Node_Interface'Class;

   overriding function Update
     (Node : Star_System_Node_Id)
      return access Node_Interface'Class;

   type Star_System_Node_Record is
     new Leaf_Node with
      record
         Ref : Harriet.Db.Star_System_Reference;
      end record;

   overriding function Contents
     (Node : Star_System_Node_Record)
      return String;

   type Universe_Record is
     new Branch_Node with null record;

   overriding function Has_Child
     (Node : Universe_Record;
      Name : String)
      return Boolean;

   overriding function Get_Child
     (Node  : Universe_Record;
      Child : String)
      return Node_Id;

   overriding procedure Bind_Child
     (Node  : in out Universe_Record;
      Name  : String;
      Child : Node_Id);

   overriding procedure Delete_Child
     (Node   : in out Universe_Record;
      Name   : String);

   overriding procedure Iterate_Children
     (Node    : Universe_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id));

   ----------------
   -- Bind_Child --
   ----------------

   overriding procedure Bind_Child
     (Node  : in out Universe_Record;
      Name  : String;
      Child : Node_Id)
   is
   begin
      raise Constraint_Error with
        "read-only filesystem";
   end Bind_Child;

   --------------
   -- Contents --
   --------------

   overriding function Contents
     (Node : Star_System_Node_Record)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for World of
        Harriet.Db.World.Select_By_Star_System
          (Node.Ref)
      loop
         Result := Result & World.Name & Character'Val (10);
      end loop;
      return To_String (Result);
   end Contents;

   ------------------
   -- Delete_Child --
   ------------------

   overriding procedure Delete_Child
     (Node   : in out Universe_Record;
      Name   : String)
   is
   begin
      raise Constraint_Error with
        "read-only filesystem";
   end Delete_Child;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Id : Star_System_Node_Id)
      return Node_Interface'Class
   is
   begin
      return Star_System_Node_Record'
        (Ref => Id.Ref);
   end Get;

   ---------------
   -- Get_Child --
   ---------------

   overriding function Get_Child
     (Node  : Universe_Record;
      Child : String)
      return Node_Id
   is
      pragma Unreferenced (Node);
   begin
      return Star_System_Node_Id'
        (Ref =>
           Harriet.Db.Star_System.First_Reference_By_Name (Child));
   end Get_Child;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Node : Universe_Record;
      Name : String)
      return Boolean
   is
      pragma Unreferenced (Node);
   begin
      return Harriet.Db.Star_System.First_Reference_By_Name (Name)
        /= Harriet.Db.Null_Star_System_Reference;
   end Has_Child;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty
     (Id : Star_System_Node_Id)
      return Boolean
   is
   begin
      return Id.Ref = Harriet.Db.Null_Star_System_Reference;
   end Is_Empty;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (Node    : Universe_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id))
   is
      pragma Unreferenced (Node);
   begin
      for Star_System of Harriet.Db.Star_System.Scan_By_Name loop
         Process (Star_System.Name,
                  Star_System_Node_Id'
                    (Ref => Star_System.Get_Star_System_Reference));
      end loop;
   end Iterate_Children;

   -------------------
   -- Universe_Node --
   -------------------

   function Universe_Node return Node_Interface'Class is
   begin
      return Universe : Universe_Record;
   end Universe_Node;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Node : Star_System_Node_Id)
      return access Node_Interface'Class
   is
      pragma Unreferenced (Node);
   begin
      return (raise Constraint_Error with
                "read-only filesystem");
   end Update;

end Harriet.File_System.Universe;
