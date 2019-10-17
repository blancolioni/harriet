with Harriet.File_System.Directories;
with Harriet.File_System.Home;
with Harriet.File_System.Proc;
with Harriet.File_System.Universe;

package body Harriet.File_System.Init is

   ----------------------------
   -- Initialize_File_System --
   ----------------------------

   procedure Initialize_File_System is
      FS : constant File_System_Interface'Class := Root_Filesystem;
      Root_Id : constant Node_Id := FS.Create (Directories.Directory_Node);
      Root    : constant access Node_Interface'Class := Root_Id.Update;
      pragma Assert (not Root.Is_Leaf);
   begin

      Root.Bind_Child
        (Name => "home",
         Child => FS.Create (Home.Home_Node));

      Root.Bind_Child
        (Name => "log",
         Child => FS.Create (Directories.Directory_Node));
      Root.Bind_Child
        (Name => "proc",
         Child => FS.Create (Proc.Create_Proc_File_System));
      Root.Bind_Child
        (Name => "etc",
         Child => FS.Create (Directories.Directory_Node));

      declare
         Mnt : constant Node_Id := FS.Create (Directories.Directory_Node);
      begin
         Root.Bind_Child ("mnt", Mnt);
         Mnt.Update.Bind_Child
           (Name  => "universe",
            Child => FS.Create (Universe.Universe_Node));
      end;

      Root.Bind_Child
        (Name => "root",
         Child => FS.Create (Directories.Directory_Node));
      Root.Bind_Child
        (Name => "tmp",
         Child => FS.Create (Directories.Directory_Node));
   end Initialize_File_System;

end Harriet.File_System.Init;
