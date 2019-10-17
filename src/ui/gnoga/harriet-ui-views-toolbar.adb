with Ada.Containers.Vectors;

--  with WL.Localisation;

with Gnoga.Gui.View;
--  with Gnoga.Gui.Element.Common;

with Concorde.UI.Views.Model_Views;

with Concorde.UI.Views.Toolbar.Localised_Button;
with Concorde.UI.Views.Toolbar.Text_Entry;
with Concorde.UI.Views.Toolbar.Toggle_Command_Button;

package body Concorde.UI.Views.Toolbar is

   package Base_View is
     new Concorde.UI.Views.Model_Views
       (Base_View_Type  => Root_View_Type,
        View_Model_Type =>
           Concorde.UI.Models.Toolbar.Root_Toolbar_Model);

   package Toolbar_Item_Vectors is
     new Ada.Containers.Vectors (Positive, Toolbar_Item);

   type Root_Toolbar_View is
     new Base_View.View_Type with
      record
         Items : Toolbar_Item_Vectors.Vector;
         Elements : Gnoga.Gui.Element.Element_Type_Array;
      end record;

   overriding procedure Create
     (View    : not null access Root_Toolbar_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String);

   overriding procedure Render
     (View : in out Root_Toolbar_View);

   overriding procedure Resize
     (View : in out Root_Toolbar_View)
   is null;

   type Toolbar_Access is access all Root_Toolbar_View;

   type Toolbar_Gnoga_View is
     new Gnoga.Gui.View.View_Type with
      record
         Toolbar : Toolbar_Access;
      end record;

   type Toolbar_Gnoga_View_Access is
     access all Toolbar_Gnoga_View'Class;

--     type Toolbar_Button_Type is
--       new Gnoga.Gui.Element.Common.Button_Type with
--        record
--           Item    : Toolbar_Item;
--           Toolbar : Toolbar_Access;
--        end record;

--     type Toolbar_Button_Access is access all Toolbar_Button_Type'Class;

--     procedure On_Toolbar_Button_Click
--       (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View    : not null access Root_Toolbar_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String)
   is
      Gnoga_View : constant Toolbar_Gnoga_View_Access :=
                     new Toolbar_Gnoga_View;
   begin
      Gnoga_View.Create (Parent, Id);
      Gnoga_View.Toolbar := Toolbar_Access (View);
      View.Create_With_Gnoga_View (Session, Gnoga_View);

      for Item of View.Items loop
         Item.Attach (Gnoga_View.all);
      end loop;
   end Create;

   ---------------------------
   -- Localised_Button_Item --
   ---------------------------

   function Localised_Button_Item
     (Session         : Concorde.Sessions.Concorde_Session;
      Key             : String;
      Command         : Concorde.UI.Models.Commands.Command_Type;
      Activation_Text : String := "";
      Layout          : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item
   is
   begin
      return Localised_Button.Create
        (Session, Key, Command, Activation_Text, Layout);
   end Localised_Button_Item;

   -----------------------------
   -- On_Toolbar_Button_Click --
   -----------------------------

--     procedure On_Toolbar_Button_Click
--       (Object : in out Gnoga.Gui.Base.Base_Type'Class)
--     is
--        Button : Toolbar_Button_Type renames Toolbar_Button_Type (Object);
--     begin
--        Button.Item.Command.On_Activated
--          (Session => Button.Toolbar.Session,
--           Value   => (if Button.Item.Text = null then ""
--                       else Button.Item.Text.all));
--     end On_Toolbar_Button_Click;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (View : in out Root_Toolbar_View)
   is
   begin
      null;
   end Render;

   ---------------------
   -- Text_Entry_Item --
   ---------------------

   function Text_Entry_Item
     (Session : Concorde.Sessions.Concorde_Session;
      Command : Concorde.UI.Models.Commands.Command_Type;
      Layout  : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item
   is
   begin
      return Concorde.UI.Views.Toolbar.Text_Entry.Create
        (Session => Session,
         Command => Command,
         Layout  => Layout);
   end Text_Entry_Item;

   ------------------------
   -- Toggle_Button_Item --
   ------------------------

   function Toggle_Button_Item
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
   begin
      return Concorde.UI.Views.Toolbar.Toggle_Command_Button.Create
        (Session       => Session,
         Base_Class    => Base_Class,
         False_Class   => False_Class,
         True_Class    => True_Class,
         False_Command => False_Command,
         True_Command  => True_Command,
         Start_State   => Start_State,
         Layout        => Layout);
   end Toggle_Button_Item;

   ------------------
   -- Toolbar_View --
   ------------------

   function Toolbar_View
     (Model : not null access
        Concorde.UI.Models.Toolbar.Root_Toolbar_Model'Class;
      Items : Toolbar_Item_Array)
      return View_Type
   is
      View : constant Toolbar_Access := new Root_Toolbar_View;
   begin
      View.Set_Model (Model);
      for Item of Items loop
         View.Items.Append (Item);
      end loop;
      return View_Type (View);
   end Toolbar_View;

end Concorde.UI.Views.Toolbar;
