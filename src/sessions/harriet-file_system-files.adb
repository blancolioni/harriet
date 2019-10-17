with Ada.Strings.Unbounded;

package body Harriet.File_System.Files is

   type File_Node_Record is
     new Leaf_Node with
      record
         Contents : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Contents
     (Node : File_Node_Record)
      return String
   is (Ada.Strings.Unbounded.To_String (Node.Contents));

   type Dynamic_File_Node_Record is
     new Leaf_Node with
      record
         Fn : File_Contents_Function;
      end record;

   overriding function Contents
     (Node : Dynamic_File_Node_Record)
      return String
   is (Node.Fn.all);

   -----------------------
   -- Dynamic_File_Node --
   -----------------------

   function Dynamic_File_Node
     (Contents : File_Contents_Function)
      return Node_Interface'Class
   is
   begin
      return Dynamic_File_Node_Record'
        (Fn => Contents);
   end Dynamic_File_Node;

   ---------------
   -- File_Node --
   ---------------

   function File_Node
     (Contents : String := "")
      return Node_Interface'Class
   is
   begin
      return File_Node_Record'
        (Contents => Ada.Strings.Unbounded.To_Unbounded_String (Contents));
   end File_Node;

end Harriet.File_System.Files;
