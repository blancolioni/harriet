with Ada.Text_IO;

with Gnoga.Gui.Element.Form;

with WL.Localisation;

with Concorde.Commands;
with Concorde.Sessions;

package body Concorde.UI.Views.Command is

   type Root_Command_View is new Root_View_Type with
      record
         Command_Form   : Gnoga.Gui.Element.Form.Form_Type;
         Command_Prompt : Gnoga.Gui.Element.Form.Label_Type;
         Command_Text   : Gnoga.Gui.Element.Form.Text_Type;
         Command_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

   overriding procedure Create
     (View    : not null access Root_Command_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String);

   overriding procedure Render
     (View : in out Root_Command_View);

   overriding function Title
     (Command : Root_Command_View)
      return String
   is ("Command");

   type Command_Access is access all Root_Command_View;

   type Gnoga_Command_View is
     new Gnoga.Gui.View.View_Type with
      record
         View : Command_Access;
      end record;

   type Gnoga_Command_View_Access is
     access all Gnoga_Command_View'Class;

   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   ------------------
   -- Command_View --
   ------------------

   function Command_View
      return View_Type
   is
   begin
      return new Root_Command_View;
   end Command_View;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View    : not null access Root_Command_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String)
   is
      Gnoga_View : constant Gnoga_Command_View_Access :=
                     new Gnoga_Command_View;
   begin
      Gnoga_View.View := Command_Access (View);
      Gnoga_View.Create (Parent, Id);

      View.Create_With_Gnoga_View (Session, Gnoga_View);

      View.Command_Form.Create
        (Parent => Gnoga_View.all);
      View.Command_Form.Class_Name ("command-form");

      View.Command_Text.Create
        (Form       => View.Command_Form);

      View.Command_Prompt.Create
        (Form       => View.Command_Form,
         Label_For  => View.Command_Text,
         Content    => WL.Localisation.Local_Text ("command-name"),
         Auto_Place => True);
      View.Command_Button.Create
        (Form  => View.Command_Form,
         Value => WL.Localisation.Local_Text ("execute"));
      Gnoga_View.On_Submit_Handler (On_Submit'Access);

   end Create;

   ---------------
   -- On_Submit --
   ---------------

   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View    : Gnoga_Command_View renames Gnoga_Command_View (Object);
   begin
      Ada.Text_IO.Put_Line
        (Concorde.Commands.Execute_Command_Line
           (Line    => View.View.Command_Text.Value,
            Session => View.View.Session));
   end On_Submit;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (View : in out Root_Command_View)
   is
   begin
      null;
   end Render;

end Concorde.UI.Views.Command;
