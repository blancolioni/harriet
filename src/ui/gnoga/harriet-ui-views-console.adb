with WL.Localisation;

with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;

with Concorde.Commands;

package body Concorde.UI.Views.Console is

   type Root_Console_View is new Root_View_Type with
      record
         Console_Form      : Gnoga.Gui.Element.Form.Form_Type;
         Console_Text      : Gnoga.Gui.Element.Common.DIV_Type;
         Console_Prompt    : Gnoga.Gui.Element.Form.Label_Type;
         Command_Text      : Gnoga.Gui.Element.Form.Text_Type;
         Command_Button    : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

   overriding procedure Create
     (View    : not null access Root_Console_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String);

   overriding procedure Render
     (View : in out Root_Console_View)
   is null;

   overriding function Title
     (Command : Root_Console_View)
      return String
   is ("Console");

   type Console_Access is access all Root_Console_View;

   type Gnoga_Console_View is
     new Gnoga.Gui.View.View_Type with
      record
         View : Console_Access;
      end record;

   type Gnoga_Console_View_Access is
     access all Gnoga_Console_View'Class;

   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   type Text_Output_Writer is
     new Concorde.Commands.Writer_Interface with
      record
         Item : Console_Access;
      end record;

   overriding procedure Put
     (Writer : Text_Output_Writer;
      Text   : String);

   overriding procedure New_Line
     (Writer : Text_Output_Writer);

   overriding procedure Put_Error
     (Writer  : Text_Output_Writer;
      Message : String);

   ------------------
   -- Console_View --
   ------------------

   function Console_View
     return View_Type
   is
   begin
      return new Root_Console_View;
   end Console_View;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View    : not null access Root_Console_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String)
   is
      Gnoga_View : constant Gnoga_Console_View_Access :=
                     new Gnoga_Console_View;
   begin
      Gnoga_View.View := Console_Access (View);
      Gnoga_View.Create (Parent, Id);

      View.Create_With_Gnoga_View (Session, Gnoga_View);

      View.Console_Form.Create
        (Parent => Gnoga_View.all);
      View.Console_Form.Class_Name ("console-form");

      View.Console_Text.Create (View.Console_Form);
      View.Console_Text.Class_Name ("console-output");

      View.Command_Text.Create
        (Form       => View.Console_Form);
      View.Command_Text.Class_Name ("console-command");

      View.Console_Prompt.Create
        (Form       => View.Console_Form,
         Label_For  => View.Command_Text,
         Content    => Session.Current_Context.Name,
         Auto_Place => True);
      View.Console_Prompt.Class_Name ("console-prompt");
      View.Command_Button.Create
        (Form  => View.Console_Form,
         Value => WL.Localisation.Local_Text ("execute-command"));
      Gnoga_View.On_Submit_Handler (On_Submit'Access);

   end Create;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Writer : Text_Output_Writer)
   is
   begin
      Writer.Item.Console_Text.New_Line;
   end New_Line;

   ---------------
   -- On_Submit --
   ---------------

   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View    : Gnoga_Console_View renames Gnoga_Console_View (Object);
      Writer  : Text_Output_Writer;
   begin
      Writer.Item := View.View;
      Writer.Item.Console_Text.Put_HTML ("<span>");
      Writer.Item.Console_Text.Put
        (View.View.Session.Current_Context.Name & "> ",
         Class => "console-prompt-text");
      Writer.Item.Console_Text.Put
        (View.View.Command_Text.Value,
         Class => "console-command-text");
      Writer.Item.Console_Text.Put_HTML ("</span>");
      Writer.Item.Console_Text.New_Line;
      Concorde.Commands.Execute_Command_Line
        (Line    => View.View.Command_Text.Value,
         Session => View.View.Session,
         Writer  => Writer);
      Writer.Item.Console_Text.Scroll_Top (Natural'Last);
      Writer.Item.Command_Text.Value ("");
      Writer.Item.Console_Prompt.Text (View.View.Session.Current_Context.Name);
   end On_Submit;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Writer : Text_Output_Writer;
      Text   : String)
   is
   begin
      Writer.Item.Console_Text.Put (Text, Class => "console-output");
   end Put;

   ---------------
   -- Put_Error --
   ---------------

   overriding procedure Put_Error
     (Writer  : Text_Output_Writer;
      Message : String)
   is
   begin
      Writer.Item.Console_Text.Put_Line
        (Message, Class => "console-error-text");
   end Put_Error;

end Concorde.UI.Views.Console;
