with WL.Localisation;

package body Concorde.UI.Views.Toolbar.Localised_Button is

   procedure On_Button_Click
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   ------------
   -- Attach --
   ------------

   overriding procedure Attach
     (Item   : in out Root_Localised_Button;
      View   : in out Gnoga.Gui.View.View_Type'Class)
   is
      use Ada.Strings.Unbounded;
   begin
      Item.Button.Create (View);
      Item.Button.Text (WL.Localisation.Local_Text (To_String (Item.Key)));
      Item.Button.On_Click_Handler (On_Button_Click'Access);
   end Attach;

   ------------
   -- Create --
   ------------

   function Create
     (Session         : Concorde.Sessions.Concorde_Session;
      Key             : String;
      Command         : Concorde.UI.Models.Commands.Command_Type;
      Activation_Text : String := "";
      Layout          : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item
   is
      use Ada.Strings.Unbounded;
      Item : constant Button_Item_Access :=
        new Root_Localised_Button'
          (Button          => <>,
           Session         => Session,
           Key             => To_Unbounded_String (Key),
           Command         => Command,
           Activation_Text => To_Unbounded_String (Activation_Text),
           Layout          => Layout);
   begin
      Item.Button.Toolbar_Item := Item;
      return Toolbar_Item (Item);
   end Create;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      use Ada.Strings.Unbounded;
      Button : Localised_Gnoga_Button renames
                 Localised_Gnoga_Button (Object);
      Item   : constant Button_Item_Access := Button.Toolbar_Item;
      Command : constant Concorde.UI.Models.Commands.Command_Type :=
                  Item.Command;
   begin
      Command.On_Activated
        (Button.Toolbar_Item.Session,
         To_String (Button.Toolbar_Item.Activation_Text));
   end On_Button_Click;

end Concorde.UI.Views.Toolbar.Localised_Button;
