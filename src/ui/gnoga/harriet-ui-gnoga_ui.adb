with Ada.Text_IO;

with Concorde.Sessions;

package body Concorde.UI.Gnoga_UI is

   ------------------------
   -- On_Connect_Default --
   ------------------------

   procedure On_Connect_Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      Session : constant Concorde.Sessions.Concorde_Session :=
                  Concorde.Sessions.New_Gnoga_Session;
   begin
      Main_Window.Connection_Data (Session);

      Session.Connect (Main_Window'Unchecked_Access);

      Ada.Text_IO.Put_Line ("connected");
   end On_Connect_Default;

   -----------------
   -- Stop_Server --
   -----------------

   procedure Stop_Server (Message : String) is
   begin
      Ada.Text_IO.Put_Line
        ("stopping server"
         & (if Message = "" then ""
           else ": " & Message));
      Concorde.Sessions.End_All_Sessions;
      Gnoga.Application.Multi_Connect.End_Application;
   end Stop_Server;

end Concorde.UI.Gnoga_UI;
