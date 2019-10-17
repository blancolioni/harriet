with Concorde.UI.Models.Commands.Execute;

package body Concorde.UI.Views.Toolbar.Toggle_Command_Button is

   procedure On_Button_Click
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   ------------
   -- Attach --
   ------------

   overriding procedure Attach
     (Item   : in out Root_Toggle_Command_Button;
      View   : in out Gnoga.Gui.View.View_Type'Class)
   is
      use Ada.Strings.Unbounded;
   begin
      Item.Container.Create (View);
      Item.Container.Class_Name ("button-container");
      Item.Button.Create (Item.Container);
      Item.Button.Class_Name (To_String (Item.Base_Class));
      Item.Button.Add_Class
        (To_String (Item.States (Item.Current_State).Class_Name));
      Item.Button.On_Click_Handler (On_Button_Click'Access);
   end Attach;

   ------------
   -- Create --
   ------------

   function Create
     (Session         : Concorde.Sessions.Concorde_Session;
      Base_Class      : String;
      False_Class     : String;
      True_Class      : String;
      False_Command   : String;
      True_Command    : String;
      Start_State     : Boolean;
      Layout          : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item
   is
      use Ada.Strings.Unbounded;
      function "+" (S : String) return Unbounded_String
                    renames To_Unbounded_String;

      Item : constant Button_Item_Access :=
               new Root_Toggle_Command_Button'
                 (Container     => <>,
                  Button        => <>,
                  Session       => Session,
                  Base_Class    => +Base_Class,
                  States        =>
                    (False => (+False_Class, +False_Command),
                     True  => (+True_Class, +True_Command)),
                  Current_State => Start_State,
                  Layout        => Layout);
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
      Button : Toggle_Gnoga_Button renames
        Toggle_Gnoga_Button (Object);
      Item   : constant Button_Item_Access := Button.Toolbar_Item;
      Command : constant Concorde.UI.Models.Commands.Command_Type :=
                  Concorde.UI.Models.Commands.Execute.Execute_Command_Line;
   begin
      Command.On_Activated
        (Item.Session, To_String (Item.States (Item.Current_State).Command));
      Button.Remove_Class
        (To_String (Item.States (Item.Current_State).Class_Name));
      Item.Current_State := not Item.Current_State;
      Button.Add_Class
        (To_String (Item.States (Item.Current_State).Class_Name));
   end On_Button_Click;

end Concorde.UI.Views.Toolbar.Toggle_Command_Button;
