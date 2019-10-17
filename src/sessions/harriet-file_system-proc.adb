with Ada.Calendar.Formatting;

with Marlowe.Version;
with Kit.Version;

with Harriet.Version;

with Harriet.Calendar;
with Harriet.Updates.Control;

with Harriet.UI;

with Harriet.File_System.Directories;
with Harriet.File_System.Files;

package body Harriet.File_System.Proc is

   function Status_Text return String;

   function Version return String
   is (Harriet.Version.Version_String);

   -----------------------------
   -- Create_Proc_File_System --
   -----------------------------

   function Create_Proc_File_System return Node_Interface'Class is
   begin
      return P : Node_Interface'Class := Directories.Directory_Node do
         P.Bind_Child
           (Name  => "status",
            Child => Root_Filesystem.Create
              (Files.Dynamic_File_Node (Status_Text'Access)));
         P.Bind_Child
           (Name  => "version",
            Child =>
              Root_Filesystem.Create
                (Files.Dynamic_File_Node (Version'Access)));
      end return;
   end Create_Proc_File_System;

   -----------------
   -- Status_Text --
   -----------------

   function Status_Text return String is
      NL                 : constant Character := Character'Val (10);
      Paused             : Boolean;
      Advance_Per_Second : Duration;
      Start_Time         : Ada.Calendar.Time;
   begin
      Harriet.Updates.Control.Get_Status
        (Start_Time, Paused, Advance_Per_Second);
      return
        Harriet.Version.Name
        & " version "
        & Harriet.Version.Version_String
        & NL
        & "kit     "
        & Kit.Version.Version_String
        & NL
        & "marlowe "
        & Marlowe.Version.Version_String
        & NL
        & "Server started "
        & Ada.Calendar.Formatting.Image (Start_Time)
        & NL
        & "status: " & (if Paused then "paused" else "running")
        & NL
        & "current server date: "
        & Harriet.Calendar.Image (Harriet.Calendar.Clock)
        & "time acceleration:"
        & Natural'Image (Natural (Advance_Per_Second));
   end Status_Text;

end Harriet.File_System.Proc;
