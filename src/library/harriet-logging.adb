with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Text_IO;

with Harriet.Calendar;
with Harriet.Options;

package body Harriet.Logging is

   Logging_Enabled : Boolean := False;
   Log_File        : Ada.Text_IO.File_Type;

   Separator       : constant Character :=
                       Ada.Characters.Latin_1.HT;

   -------------------
   -- Finish_Update --
   -------------------

   procedure Finish_Update is
   begin
      if Logging_Enabled then
         Ada.Text_IO.Flush (Log_File);
      end if;
   end Finish_Update;

   ---------
   -- Log --
   ---------

   procedure Log
     (Category : String;
      Message  : String)
   is
   begin
      if Logging_Enabled then
         Ada.Text_IO.Put_Line
           (Log_File,
            Harriet.Calendar.Image
              (Harriet.Calendar.Clock,
               Include_Time_Fraction => True)
            & Separator
            & Category
            & Separator
            & Message);
      end if;
   end Log;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging is
      Log_Directory : constant String :=
                        Harriet.Options.Log_Folder;
   begin
      if not Ada.Directories.Exists (Log_Directory) then
         Ada.Directories.Create_Directory (Log_Directory);
      end if;
      Ada.Text_IO.Create (Log_File, Ada.Text_IO.Out_File,
                          Log_Directory & "/harriet.log");
      Logging_Enabled := True;
   end Start_Logging;

   ------------------
   -- Start_Update --
   ------------------

   procedure Start_Update is
   begin
      null;
   end Start_Update;

   ------------------
   -- Stop_Logging --
   ------------------

   procedure Stop_Logging is
   begin
      if Logging_Enabled then
         Ada.Text_IO.Close (Log_File);
         Logging_Enabled := False;
      end if;
   end Stop_Logging;

end Harriet.Logging;
