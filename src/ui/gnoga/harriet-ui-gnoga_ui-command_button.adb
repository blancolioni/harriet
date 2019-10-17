with Ada.Text_IO;

package body Concorde.UI.Gnoga_UI.Command_Button is

   procedure On_Click (Object  : in out Gnoga.Gui.Base.Base_Type'Class);

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (Button  : in out Command_Button;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Content : in     String := "";
      ID      : in     String := "")
   is
   begin
      Gnoga.Gui.Element.Common.Button_Type (Button).Create
        (Parent, Content, ID);
      Button.On_Click_Handler (On_Click'Access);
   end Create;

   --------------
   -- On_Click --
   --------------

   procedure On_Click
     (Object  : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      Button : Command_Button'Class renames Command_Button'Class (Object);
   begin
      Ada.Text_IO.Put_Line
        (Button.Command.Execute
           (Session   => null,
            Arguments => Concorde.Commands.No_Arguments));
   end On_Click;

end Concorde.UI.Gnoga_UI.Command_Button;
