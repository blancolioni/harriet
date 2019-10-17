with Ada.Strings.Unbounded;

with Gnoga.Gui.Element.Common;

with Concorde.Sessions;

private package Concorde.UI.Views.Toolbar.Toggle_Command_Button is

   type Root_Toggle_Command_Button is
     limited new Toolbar_Item_Interface with private;

   overriding procedure Attach
     (Item   : in out Root_Toggle_Command_Button;
      View   : in out Gnoga.Gui.View.View_Type'Class);

   function Create
     (Session         : Concorde.Sessions.Concorde_Session;
      Base_Class      : String;
      False_Class     : String;
      True_Class      : String;
      False_Command   : String;
      True_Command    : String;
      Start_State     : Boolean;
      Layout          : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item;

private

   type Button_Item_Access is access all Root_Toggle_Command_Button'Class;

   type Toggle_Gnoga_Button is
     new Gnoga.Gui.Element.Common.DIV_Type with
      record
         Toolbar_Item : Button_Item_Access;
      end record;

   type State_Record is
      record
         Class_Name : Ada.Strings.Unbounded.Unbounded_String;
         Command    : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Boolean_State_Array is
     array (Boolean) of State_Record;

   type Root_Toggle_Command_Button is
   limited new Toolbar_Item_Interface with
      record
         Container     : Gnoga.Gui.Element.Common.DIV_Type;
         Button        : Toggle_Gnoga_Button;
         Session       : Concorde.Sessions.Concorde_Session;
         Base_Class    : Ada.Strings.Unbounded.Unbounded_String;
         States        : Boolean_State_Array;
         Current_State : Boolean;
         Layout        : Toolbar_Item_Layout;
      end record;

end Concorde.UI.Views.Toolbar.Toggle_Command_Button;
