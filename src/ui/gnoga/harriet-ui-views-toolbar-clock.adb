with Gnoga.Gui.Element.Common;

with Concorde.Calendar;
with Concorde.Signals;

package body Concorde.UI.Views.Toolbar.Clock is

   type Root_Clock_Item;

   type Clock_Item_Access is access all Root_Clock_Item'Class;

   type Gnoga_Clock_Label is
     new Gnoga.Gui.Element.Common.Span_Type with
      record
         Toolbar_Item : Clock_Item_Access;
      end record;

   type Root_Clock_Item is limited new Toolbar_Item_Interface with
      record
         Clock_Handler_Id : Concorde.Signals.Handler_Id :=
                              Concorde.Signals.Null_Handler_Id;
         Label            : Gnoga_Clock_Label;
         Session          : Concorde.Sessions.Concorde_Session;
         Layout           : Toolbar_Item_Layout;
      end record;

   overriding procedure Attach
     (Item   : in out Root_Clock_Item;
      View   : in out Gnoga.Gui.View.View_Type'Class);

   type Clock_Update_Data is
     new Concorde.Signals.Signal_Data_Interface with
      record
         Item : Clock_Item_Access;
      end record;

   procedure Handle_Clock_Tick
     (Object : Concorde.Signals.Signaler'Class;
      Data   : Concorde.Signals.Signal_Data_Interface'Class);

   ------------
   -- Attach --
   ------------

   overriding procedure Attach
     (Item   : in out Root_Clock_Item;
      View   : in out Gnoga.Gui.View.View_Type'Class)
   is
   begin
      Item.Label.Create
        (View,
         Concorde.Calendar.Image (Concorde.Calendar.Clock));
   end Attach;

   ------------
   -- Create --
   ------------

   function Create
     (Session         : Concorde.Sessions.Concorde_Session;
      Layout          : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item
   is
      Item : constant Clock_Item_Access :=
               new Root_Clock_Item'
                 (Label            => <>,
                  Clock_Handler_Id => <>,
                  Session          => Session,
                  Layout           => Layout);
      Data : constant Clock_Update_Data := (Item => Item);
   begin
      Item.Label.Toolbar_Item := Item;
      Item.Clock_Handler_Id :=
        Session.Add_Handler
          (Signal  => Concorde.Sessions.Signal_Clock_Tick,
           Handler => Handle_Clock_Tick'Access,
           Data    => Data);
      return Toolbar_Item (Item);
   end Create;

   -----------------------
   -- Handle_Clock_Tick --
   -----------------------

   procedure Handle_Clock_Tick
     (Object : Concorde.Signals.Signaler'Class;
      Data   : Concorde.Signals.Signal_Data_Interface'Class)
   is
      pragma Unreferenced (Object);
      Clock_Data : Clock_Update_Data renames Clock_Update_Data (Data);
   begin
      Clock_Data.Item.Label.Text
        (Concorde.Calendar.Image (Concorde.Calendar.Clock));
   end Handle_Clock_Tick;

end Concorde.UI.Views.Toolbar.Clock;
