with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;

package Concorde.UI.Gnoga_UI is

   procedure On_Connect_Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure Stop_Server (Message : String);

end Concorde.UI.Gnoga_UI;
