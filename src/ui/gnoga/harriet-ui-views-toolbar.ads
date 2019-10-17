with Concorde.UI.Models.Commands;
with Concorde.UI.Models.Toolbar;

package Concorde.UI.Views.Toolbar is

   type Toolbar_Item_Layout is private;

   Default_Layout    : constant Toolbar_Item_Layout;
   Right_Side_Layout : constant Toolbar_Item_Layout;

   type Toolbar_Item_Interface is limited interface;

   procedure Attach
     (Item   : in out Toolbar_Item_Interface;
      View   : in out Gnoga.Gui.View.View_Type'Class)
      is abstract;

   type Toolbar_Item is access all Toolbar_Item_Interface'Class;

   type Toolbar_Item_Array is array (Positive range <>) of Toolbar_Item;

   function Localised_Button_Item
     (Session         : Concorde.Sessions.Concorde_Session;
      Key             : String;
      Command         : Concorde.UI.Models.Commands.Command_Type;
      Activation_Text : String := "";
      Layout          : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item;

   function Toggle_Button_Item
     (Session         : Concorde.Sessions.Concorde_Session;
      Base_Class      : String;
      False_Class     : String;
      True_Class      : String;
      False_Command   : String;
      True_Command    : String;
      Start_State     : Boolean;
      Layout          : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item;

   --     function Localised_Label_Item
--       (Key    : String;
--        Layout : Toolbar_Item_Layout := Default_Layout)
--        return Toolbar_Item;
--
--     function Text_Label_Item
--       (Text   : String;
--        Layout : Toolbar_Item_Layout := Default_Layout)
--        return Toolbar_Item;
--

   function Text_Entry_Item
     (Session : Concorde.Sessions.Concorde_Session;
      Command : Concorde.UI.Models.Commands.Command_Type;
      Layout  : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item;

   function Toolbar_View
     (Model : not null access
        Concorde.UI.Models.Toolbar.Root_Toolbar_Model'Class;
      Items : Toolbar_Item_Array)
      return View_Type;

private

   type Item_Gravity is (Left, Right);

   type Toolbar_Item_Layout is
      record
         Gravity : Item_Gravity := Left;
      end record;

   Default_Layout    : constant Toolbar_Item_Layout := (others => <>);
   Right_Side_Layout : constant Toolbar_Item_Layout := (Gravity => Right);

--     type Toolbar_Item_Class is (Button_Item, Label_Item, Text_Entry_Item);
--
--     type Toolbar_Item is
--        record
--           Class   : Toolbar_Item_Class;
--           Key     : access String;
--           Text    : access String;
--           Command : Concorde.UI.Models.Commands.Command_Type;
--           Layout  : Toolbar_Item_Layout;
--        end record;
--
--     function Localised_Button_Item
--       (Key             : String;
--        Command         : Concorde.UI.Models.Commands.Command_Type;
--        Activation_Text : String := "";
--        Layout          : Toolbar_Item_Layout := Default_Layout)
--        return Toolbar_Item
--     is (Toolbar_Item'
--           (Class   => Button_Item,
--            Key     => new String'(Key),
--            Text    => new String'(Activation_Text),
--            Command => Command,
--            Layout  => Layout));
--
--     function Localised_Label_Item
--       (Key    : String;
--        Layout : Toolbar_Item_Layout := Default_Layout)
--        return Toolbar_Item
--     is (Toolbar_Item'
--           (Class   => Label_Item,
--            Key     => new String'(Key),
--            Text    => null,
--            Command => null,
--            Layout  => Layout));
--
--     function Text_Label_Item
--       (Text   : String;
--        Layout : Toolbar_Item_Layout := Default_Layout)
--        return Toolbar_Item
--     is (Toolbar_Item'
--           (Class   => Label_Item,
--            Key     => null,
--            Text    => new String'(Text),
--            Command => null,
--            Layout  => Layout));
--
--     function Text_Entry_Item
--       (Command : Concorde.UI.Models.Commands.Command_Type;
--        Layout  : Toolbar_Item_Layout := Default_Layout)
--        return Toolbar_Item
--     is (Toolbar_Item'
--           (Class   => Text_Entry_Item,
--            Key     => null,
--            Text    => null,
--            Command => Command,
--            Layout  => Layout));

end Concorde.UI.Views.Toolbar;
