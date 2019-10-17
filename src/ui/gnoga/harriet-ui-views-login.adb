with Ada.Text_IO;

with Gnoga.Gui.Element.Form;
with Gnoga.Gui.View.Console;

with WL.Localisation;

with Concorde.UI.Views.Model_Views;

package body Concorde.UI.Views.Login is

   package Base_View is
     new Concorde.UI.Views.Model_Views
       (Base_View_Type  => Root_View_Type,
        View_Model_Type => Concorde.UI.Models.Login.Root_Login_Model);

   type Root_Login_View is
     new Base_View.View_Type with
      record
         Login_Form      : Gnoga.Gui.Element.Form.Form_Type;
         Login_Prompt    : Gnoga.Gui.Element.Form.Label_Type;
         Login_Name      : Gnoga.Gui.Element.Form.Text_Type;
         Password_Prompt : Gnoga.Gui.Element.Form.Label_Type;
         Password_Text   : Gnoga.Gui.Element.Form.Text_Type;
         Login_Button    : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

   overriding procedure Create
     (View    : not null access Root_Login_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String);

   overriding function Accepts_Text_Entry
     (View : Root_Login_View)
      return Boolean;

   overriding procedure Render
     (View : in out Root_Login_View);

   overriding procedure Resize
     (View : in out Root_Login_View)
   is null;

   type Login_Access is access all Root_Login_View;

   type Login_Gnoga_View is
     new Gnoga.Gui.View.Console.Console_View_Type with
      record
         Login : Login_Access;
      end record;

   type Login_Gnoga_View_Access is access all Login_Gnoga_View'Class;

   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   ------------------------
   -- Accepts_Text_Entry --
   ------------------------

   overriding function Accepts_Text_Entry
     (View : Root_Login_View)
      return Boolean
   is
      pragma Unreferenced (View);
   begin
      return True;
   end Accepts_Text_Entry;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View    : not null access Root_Login_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String)
   is
      Gnoga_View : constant Login_Gnoga_View_Access :=
                     new Login_Gnoga_View;
   begin
      Gnoga_View.Create (Parent, Id);
      Gnoga_View.Login := Login_Access (View);
      View.Create_With_Gnoga_View (Session, Gnoga_View);

      View.Login_Form.Create
        (Parent => Gnoga_View.all,
         ID     => "login-form");
      View.Login_Form.Class_Name ("login-form");
      View.Login_Name.Create
        (Form => View.Login_Form,
         ID         => "login-text");
      View.Login_Prompt.Create
        (Form       => View.Login_Form,
         Label_For  => View.Login_Name,
         Content    => WL.Localisation.Local_Text ("login-name"),
         ID         => "login-prompt",
         Auto_Place => True);
      View.Password_Text.Create
        (Form => View.Login_Form,
         ID         => "password-text");
      View.Password_Prompt.Create
        (Form       => View.Login_Form,
         ID         => "password-prompt",
         Label_For  => View.Password_Text,
         Content    => WL.Localisation.Local_Text ("password"),
         Auto_Place => True);
      View.Login_Button.Create
        (Form  => View.Login_Form,
         Value => WL.Localisation.Local_Text ("login"),
         ID    => "login-button");
      View.Login_Name.Focus;

      Gnoga_View.On_Submit_Handler (On_Submit'Access);

   end Create;

   ----------------
   -- Login_View --
   ----------------

   function Login_View
     (Model : not null access Concorde.UI.Models.Login.Root_Login_Model'Class)
      return View_Type
   is
      View : constant Login_Access := new Root_Login_View;
   begin
      View.Set_Model (Model);
      return View_Type (View);
   end Login_View;

   ---------------
   -- On_Submit --
   ---------------

   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : Login_Gnoga_View renames Login_Gnoga_View (Object);
      Success : constant Boolean :=
                  View.Login.Model.Login
                    (View.Login.Login_Name.Value,
                     View.Login.Password_Text.Value);
   begin
      if Success then
         Ada.Text_IO.Put_Line ("Login successful");
      else
         Ada.Text_IO.Put_Line ("Login failed");
      end if;
   end On_Submit;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (View : in out Root_Login_View)
   is
   begin
      null;
   end Render;

end Concorde.UI.Views.Login;
