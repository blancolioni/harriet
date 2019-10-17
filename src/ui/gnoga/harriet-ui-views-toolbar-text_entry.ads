private with Gnoga.Gui.Element.Common;
private with Gnoga.Gui.Element.Form;

with Concorde.Sessions;

private package Concorde.UI.Views.Toolbar.Text_Entry is

   type Root_Text_Entry is
     limited new Toolbar_Item_Interface with private;

   overriding procedure Attach
     (Item   : in out Root_Text_Entry;
      View   : in out Gnoga.Gui.View.View_Type'Class);

   function Create
     (Session         : Concorde.Sessions.Concorde_Session;
      Command         : Concorde.UI.Models.Commands.Command_Type;
      Layout          : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item;

private

   type Text_Entry_Access is access all Root_Text_Entry'Class;

   type Gnoga_Text_Entry is
     new Gnoga.Gui.Element.Form.Form_Type with
      record
         Text_Entry_Item : Text_Entry_Access;
         Input           : Gnoga.Gui.Element.Form.Text_Type;
         Button          : Gnoga.Gui.Element.Form.Submit_Button_Type;
         Output          : Gnoga.Gui.Element.Common.DIV_Type;
      end record;

   type Root_Text_Entry is
   limited new Toolbar_Item_Interface with
      record
         Text_Entry      : Gnoga_Text_Entry;
         Session         : Concorde.Sessions.Concorde_Session;
         Command         : Concorde.UI.Models.Commands.Command_Type;
         Layout          : Toolbar_Item_Layout;
      end record;

end Concorde.UI.Views.Toolbar.Text_Entry;
