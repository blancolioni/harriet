package Harriet.File_System is

   type Node_Id_Interface is interface;
   subtype Node_Id is Node_Id_Interface'Class;

   function Is_Empty (Id : Node_Id_Interface) return Boolean is abstract;

   type Node_Interface is interface;

   function Contents
     (Node : Node_Interface)
      return String
      is abstract;

   function Is_Leaf
     (Node : Node_Interface)
      return Boolean
      is abstract;

   function Has_Child
     (Node : Node_Interface;
      Name : String)
      return Boolean
      is abstract
     with Pre'Class => not Is_Leaf (Node);

   function Get_Child
     (Node  : Node_Interface;
      Child : String)
      return Node_Id
      is abstract
     with Pre'Class => not Is_Leaf (Node);

   procedure Bind_Child
     (Node  : in out Node_Interface;
      Name  : String;
      Child : Node_Id)
   is abstract
     with Pre'Class => not Is_Leaf (Node);

   procedure Delete_Child
     (Node   : in out Node_Interface;
      Name   : String)
   is abstract
     with Pre'Class => not Is_Leaf (Node)
     and then Has_Child (Node_Interface'Class (Node), Name);

   procedure Iterate_Children
     (Node    : Node_Interface;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id))
   is abstract
     with Pre'Class => not Is_Leaf (Node);

   function Get
     (Id : Node_Id_Interface)
      return Node_Interface'Class
      is abstract;

   function Update
     (Node : Node_Id_Interface)
      return access Node_Interface'Class
      is abstract;

   type Read_Only_Node_Id is
     abstract new Node_Id_Interface with private;

   overriding function Update
     (Node : Read_Only_Node_Id)
      return access Node_Interface'Class;

   type File_System_Interface is interface;

   function Get_Root_Node_Id
     (FS : File_System_Interface)
      return Node_Id
      is abstract;

   function Create
     (FS   : File_System_Interface;
      Node : Node_Interface'Class)
      return Node_Id
      is abstract;

   procedure Delete
     (FS   : File_System_Interface;
      Node : Node_Id)
   is abstract;

   function Root_Filesystem return File_System_Interface'Class;

   type Leaf_Node is
     abstract new Node_Interface with private;

   type Branch_Node is
     abstract new Node_Interface with private;

private

   type Read_Only_Node_Id is
     abstract new Node_Id_Interface with null record;

   type Leaf_Node is
     abstract new Node_Interface with null record;

   overriding function Is_Leaf
     (Node : Leaf_Node)
      return Boolean;

   overriding function Has_Child
     (Node : Leaf_Node;
      Name : String)
      return Boolean;

   overriding function Get_Child
     (Node  : Leaf_Node;
      Child : String)
      return Node_Id;

   overriding procedure Bind_Child
     (Node  : in out Leaf_Node;
      Name  : String;
      Child : Node_Id);

   overriding procedure Delete_Child
     (Node   : in out Leaf_Node;
      Name   : String);

   overriding procedure Iterate_Children
     (Node    : Leaf_Node;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id));

   type Branch_Node is
     abstract new Node_Interface with null record;

   overriding function Is_Leaf
     (Node : Branch_Node)
      return Boolean;

   overriding function Contents
     (Node : Branch_Node)
      return String;

end Harriet.File_System;
