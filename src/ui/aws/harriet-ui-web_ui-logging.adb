with Ada.Calendar.Formatting;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Messages;

package body Harriet.UI.Web_UI.Logging is

   Started : Boolean := False;
   Field_Separator : constant Character := Character'Val (9);

   type Request_Field_Get_Function is
     access function (Request : AWS.Status.Data) return String;

   type Response_Field_Get_Function is
     access function (Response : AWS.Response.Data) return String;

   type Log_Field (Name_Length : Positive) is
      record
         Field_Name : String (1 .. Name_Length);
         Request    : Request_Field_Get_Function;
         Response   : Response_Field_Get_Function;
      end record;

   function Field
     (Name    : String;
      Request : Request_Field_Get_Function)
      return Log_Field
   is (Name'Length, Name, Request, null);

   function Field
     (Name     : String;
      Response : Response_Field_Get_Function)
      return Log_Field
   is (Name'Length, Name, null, Response);

   procedure Check_Start;

   procedure Log (Line : String);

   package Field_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Log_Field);

   Logged_Fields : Field_Lists.List;

   function Get_Method (Request : AWS.Status.Data) return String
   is (AWS.Status.Request_Method'Image
       (AWS.Status.Method (Request)));

   function Get_Uri (Request : AWS.Status.Data) return String
   is (AWS.Status.URI (Request));

   function Get_Status (Response : AWS.Response.Data) return String
   is (AWS.Messages.Status_Code'Image (AWS.Response.Status_Code (Response)));

   -----------------
   -- Check_Start --
   -----------------

   procedure Check_Start is

      procedure Field
        (Name : String;
         Get  : Request_Field_Get_Function);

      procedure Field
        (Name : String;
         Get  : Response_Field_Get_Function);

      -----------
      -- Field --
      -----------

      procedure Field
        (Name : String;
         Get  : Request_Field_Get_Function)
      is
      begin
         Logged_Fields.Append (Field (Name, Get));
      end Field;

      -----------
      -- Field --
      -----------

      procedure Field
        (Name : String;
         Get  : Response_Field_Get_Function)
      is
      begin
         Logged_Fields.Append (Field (Name, Get));
      end Field;

   begin
      if not Started then
         Started := True;

         Field ("Method", Get_Method'Access);
         Field ("URI", Get_Uri'Access);
         Field ("Status", Get_Status'Access);

         declare
            use Ada.Strings.Unbounded;
            Line : Unbounded_String := To_Unbounded_String ("Time");
         begin
            for F of Logged_Fields loop
               Line := Line & Field_Separator & F.Field_Name;
            end loop;
            Log (To_String (Line));
         end;
      end if;
   end Check_Start;

   ---------
   -- Log --
   ---------

   procedure Log (Line : String) is
      Path : constant String :=
        "log/server.log";
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Directories.Create_Path ("log");

      if Ada.Directories.Exists (Path) then
         Ada.Text_IO.Open
           (File, Ada.Text_IO.Append_File, Path);
      else
         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File, Path);
      end if;
      Ada.Text_IO.Put_Line (File, Line);
      Ada.Text_IO.Close (File);
   end Log;

   -----------------
   -- Log_Request --
   -----------------

   procedure Log_Request
     (Request  : AWS.Status.Data;
      Response : AWS.Response.Data)
   is
      use Ada.Strings.Unbounded;
      Line : Unbounded_String :=
        To_Unbounded_String
          (Ada.Calendar.Formatting.Image
             (Date                  => Ada.Calendar.Clock,
              Include_Time_Fraction => True));
   begin
      Check_Start;
      for Field of Logged_Fields loop
         Line := Line & Field_Separator;

         if Field.Request /= null then
            Line := Line & Field.Request (Request);
         elsif Field.Response /= null then
            Line := Line & Field.Response (Response);
         end if;
      end loop;
      Log (To_String (Line));

   end Log_Request;

   -----------------
   -- On_Starting --
   -----------------

   procedure On_Starting is
   begin
      Log (Ada.Calendar.Formatting.Image (Ada.Calendar.Clock, True)
           & ": starting");
      Check_Start;
   end On_Starting;

   -------------
   -- On_Stop --
   -------------

   procedure On_Stop is
   begin
      Log (Ada.Calendar.Formatting.Image (Ada.Calendar.Clock, True)
           & ": stopped");
   end On_Stop;

   -----------------
   -- On_Stopping --
   -----------------

   procedure On_Stopping (Message : String) is
   begin
      Log (Ada.Calendar.Formatting.Image (Ada.Calendar.Clock, True)
           & ": stopping: " & Message);
   end On_Stopping;

end Harriet.UI.Web_UI.Logging;
