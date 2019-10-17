with WL.Localisation;

with Concorde.Commands;

package body Concorde.UI.Views.Toolbar.Text_Entry is

   type Text_Output_Writer is
     new Concorde.Commands.Writer_Interface with
      record
         Item : Text_Entry_Access;
      end record;

   overriding procedure Put
     (Writer : Text_Output_Writer;
      Text   : String);

   overriding procedure New_Line
     (Writer : Text_Output_Writer);

   overriding procedure Put_Error
     (Writer  : Text_Output_Writer;
      Message : String);

   procedure On_Form_Submit
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   ------------
   -- Attach --
   ------------

   overriding procedure Attach
     (Item   : in out Root_Text_Entry;
      View   : in out Gnoga.Gui.View.View_Type'Class)
   is
   begin
      Item.Text_Entry.Create (View);
      Item.Text_Entry.Input.Create (Item.Text_Entry);
      Item.Text_Entry.Button.Create
        (Item.Text_Entry,  WL.Localisation.Local_Text ("execute-command"));
      Item.Text_Entry.Output.Create (Item.Text_Entry);
      Item.Text_Entry.Output.Class_Name ("toolbar-text-entry-output");
      Item.Text_Entry.On_Submit_Handler (On_Form_Submit'Access);
   end Attach;

   ------------
   -- Create --
   ------------

   function Create
     (Session         : Concorde.Sessions.Concorde_Session;
      Command         : Concorde.UI.Models.Commands.Command_Type;
      Layout          : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item
   is
      Item : constant Text_Entry_Access := new Root_Text_Entry'
        (Text_Entry => <>,
         Session    => Session,
         Command    => Command,
         Layout     => Layout);
   begin
      Item.Text_Entry.Text_Entry_Item := Item;
      return Toolbar_Item (Item);
   end Create;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Writer : Text_Output_Writer)
   is
   begin
      Writer.Item.Text_Entry.Output.New_Line;
   end New_Line;

   --------------------
   -- On_Form_Submit --
   --------------------

   procedure On_Form_Submit
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      Gnoga_Item : Gnoga_Text_Entry renames Gnoga_Text_Entry (Object);
   begin
      if Gnoga_Item.Input.Value /= "" then
         declare
            Writer : Text_Output_Writer;
         begin
            Writer.Item := Gnoga_Item.Text_Entry_Item;
            Writer.Item.Text_Entry.Output.Text ("");
            Concorde.Commands.Execute_Command_Line
              (Line    => Gnoga_Item.Input.Value,
               Session => Gnoga_Item.Text_Entry_Item.Session,
               Writer  => Writer);
            Gnoga_Item.Input.Value ("");
         end;
      end if;
   end On_Form_Submit;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Writer : Text_Output_Writer;
      Text   : String)
   is
   begin
      Writer.Item.Text_Entry.Output.Put (Text);
   end Put;

   ---------------
   -- Put_Error --
   ---------------

   overriding procedure Put_Error
     (Writer  : Text_Output_Writer;
      Message : String)
   is
   begin
      Writer.Item.Text_Entry.Output.Put_Line (Message, "error-text");
   end Put_Error;

end Concorde.UI.Views.Toolbar.Text_Entry;
