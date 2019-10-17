package body Harriet.File_System.Db_FS is

   type Table_Record_Node_Id is
     new Read_Only_Node_Id with
      record
         Ref : Record_Reference := Null_Record_Reference;
      end record;

   overriding function Is_Empty
     (Item : Table_Record_Node_Id)
      return Boolean
   is (Item.Ref = Null_Record_Reference);

   overriding function Get
     (Id : Table_Record_Node_Id)
      return Node_Interface'Class;

   type Table_Record_Node is
     new Leaf_Node with
      record
         Ref : Record_Reference;
      end record;

   overriding function Contents
     (Node : Table_Record_Node)
      return String;

   type Container_Node_Record is
     new Branch_Node with
      record
           Handle : Container_Handle;
      end record;

   overriding function Has_Child
     (Node : Container_Node_Record;
      Name : String)
      return Boolean;

   overriding function Get_Child
     (Node : Container_Node_Record;
      Name : String)
      return Node_Id_Interface'Class;

   overriding procedure Iterate_Children
     (Node    : Container_Node_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id));

   overriding procedure Bind_Child
     (Node  : in out Container_Node_Record;
      Name  : String;
      Child : Node_Id);

   overriding procedure Delete_Child
     (Node   : in out Container_Node_Record;
      Name   : String);

   type Container_Node_Id_Record is
     new Read_Only_Node_Id with
      record
         Handle : Container_Handle;
      end record;

   overriding function Is_Empty
     (Node : Container_Node_Id_Record)
      return Boolean
   is (False);

   overriding function Get
     (Node : Container_Node_Id_Record)
      return Node_Interface'Class;

   ----------------
   -- Bind_Child --
   ----------------

   overriding procedure Bind_Child
     (Node  : in out Container_Node_Record;
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

   overriding function Contents (Node : Table_Record_Node) return String is
      Rec : constant Record_Interface'Class :=
        Get_Record (Node.Ref);
   begin
      return Contents (Rec);
   end Contents;

   ------------------
   -- Delete_Child --
   ------------------

   overriding procedure Delete_Child
     (Node   : in out Container_Node_Record;
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
     (Id : Table_Record_Node_Id)
      return Node_Interface'Class
   is
   begin
      return Table_Record_Node'
        (Ref => Id.Ref);
   end Get;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Node : Container_Node_Id_Record)
      return Node_Interface'Class
   is
   begin
      return Container_Node_Record'
        (Handle => Node.Handle);
   end Get;

   ---------------
   -- Get_Child --
   ---------------

   overriding function Get_Child
     (Node : Container_Node_Record;
      Name : String)
      return Node_Id_Interface'Class
   is
   begin
      return Table_Record_Node_Id'
        (Ref => Get_Reference_By_Name (Node.Handle, Name));
   end Get_Child;

   ---------------------------
   -- Get_Container_Node_Id --
   ---------------------------

   function Get_Container_Node_Id
     (Handle : Container_Handle)
      return Node_Id_Interface'Class
   is
   begin
      return Container_Node_Id_Record'
        (Handle => Handle);
   end Get_Container_Node_Id;

   ------------------------
   -- Get_Record_Node_Id --
   ------------------------

   function Get_Record_Node_Id (Reference : Record_Reference) return Node_Id is
   begin
      return Table_Record_Node_Id'(Ref => Reference);
   end Get_Record_Node_Id;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Node : Container_Node_Record;
      Name : String)
      return Boolean
   is
   begin
      return Get_Reference_By_Name (Node.Handle, Name)
        /= Null_Record_Reference;
   end Has_Child;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (Node    : Container_Node_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id))
   is
      procedure Internal_Process (Item : Record_Interface'Class);

      ----------------------
      -- Internal_Process --
      ----------------------

      procedure Internal_Process (Item : Record_Interface'Class) is
      begin
         Process (Item.Name,
                  Table_Record_Node_Id'
                    (Ref => Get_Reference (Item)));
      end Internal_Process;

   begin
      Iterate (Node.Handle, Internal_Process'Access);
   end Iterate_Children;

end Harriet.File_System.Db_FS;
