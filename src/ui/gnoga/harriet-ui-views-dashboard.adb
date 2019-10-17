with Ada.Containers.Doubly_Linked_Lists;

with Gnoga.Gui.Element.Common;
with Gnoga.Gui.View.Docker;

--  with Concorde.Factions;

with Concorde.UI.Layouts;

with Concorde.UI.Models.Toolbar;
with Concorde.UI.Models.Commands.Execute;

with Concorde.UI.Views.Model_Views;
with Concorde.UI.Views.Toolbar;
with Concorde.UI.Views.Toolbar.Clock;

package body Concorde.UI.Views.Dashboard is

   type Root_Cell_View_Type is
     new Root_View_Type with
      record
         Cell_Contents : View_Type;
         Tile          : Concorde.UI.Layouts.Tile_Index;
      end record;

   overriding procedure Create
     (View    : not null access Root_Cell_View_Type;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String);

   overriding procedure Close
     (View : in out Root_Cell_View_Type);

   overriding procedure Render
     (View : in out Root_Cell_View_Type)
   is null;

   overriding procedure Resize
     (View : in out Root_Cell_View_Type);

   overriding function Title
     (View : Root_Cell_View_Type)
      return String
   is ("dashboard");

   procedure Set_Layout
     (View          : not null access Root_Cell_View_Type'Class;
      Left, Top     : Natural;
      Width, Height : Natural);

   type Cell_View_Type is access all Root_Cell_View_Type;

   type Cell_Gnoga_View is
     new Gnoga.Gui.View.View_Type with
      record
         Cell : Cell_View_Type;
      end record;

   type Cell_Gnoga_View_Access is access all Cell_Gnoga_View'Class;

   package Base_View is
     new Concorde.UI.Views.Model_Views
       (Base_View_Type  => Root_View_Type,
        View_Model_Type => Concorde.UI.Models.Dashboard.Root_Dashboard_Model);

   package Cell_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Cell_View_Type);

   type Root_Dashboard_View is
     new Base_View.View_Type with
      record
         Top_Toolbar : Concorde.UI.Views.View_Type;
         Body_Div    : Gnoga.Gui.Element.Common.DIV_Access;
         Cells       : Cell_Lists.List;
         Active_Cell : Cell_View_Type := null;
      end record;

   overriding procedure Create
     (View    : not null access Root_Dashboard_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String);

   overriding procedure Close
     (View : in out Root_Dashboard_View);

   overriding procedure Render
     (View : in out Root_Dashboard_View);

   overriding procedure Resize
     (View : in out Root_Dashboard_View);

   overriding procedure Model_Changed
     (View : in out Root_Dashboard_View);

   overriding procedure Add_Child
     (Dashboard : in out Root_Dashboard_View;
      Child     : not null access Root_View_Type'Class);

   procedure Set_Active_Cell
     (Dashboard : in out Root_Dashboard_View'Class;
      Cell      : Cell_View_Type);

   overriding procedure Handle_Key_Press
     (View    : in out Root_Dashboard_View;
      Meta    : Boolean;
      Alt     : Boolean;
      Control : Boolean;
      Shift   : Boolean;
      Key     : Natural);

   function Cell
     (View  : Root_Dashboard_View'Class;
      Index : Concorde.UI.Layouts.Tile_Index)
      return Cell_View_Type;

   type Dashboard_Access is access all Root_Dashboard_View;

   type Dashboard_Gnoga_View is
     new Gnoga.Gui.View.Docker.Docker_View_Type with
      record
         Dashboard : Dashboard_Access;
      end record;

   type Dashboard_Gnoga_View_Access is access all Dashboard_Gnoga_View'Class;

   procedure Create_Toolbar
     (View   : not null access Root_Dashboard_View'Class;
      Gnoga_View : Dashboard_Gnoga_View_Access);

   procedure Create_Layout
     (View       : not null access Root_Dashboard_View'Class;
      Gnoga_View : Dashboard_Gnoga_View_Access);

   type Dashboard_Command_Type is
     (No_Command,
      Move_Focus_Command, Swap_Tiles_Command, Delete_Tile_Command);

   type Dashboard_Command (Command : Dashboard_Command_Type := No_Command) is
      record
         case Command is
            when No_Command =>
               null;
            when Move_Focus_Command | Swap_Tiles_Command =>
               Direction : Concorde.UI.Layouts.Layout_Direction;
            when Delete_Tile_Command =>
               null;
         end case;
      end record;

   Command_Bindings : constant array (0 .. 255) of Dashboard_Command :=
                        (104 =>
                           (Move_Focus_Command, Concorde.UI.Layouts.West),
                         107 =>
                           (Move_Focus_Command, Concorde.UI.Layouts.North),
                         108 =>
                           (Move_Focus_Command, Concorde.UI.Layouts.East),
                         106 =>
                           (Move_Focus_Command, Concorde.UI.Layouts.South),
                         72    =>
                           (Swap_Tiles_Command, Concorde.UI.Layouts.West),
                         75    =>
                           (Swap_Tiles_Command, Concorde.UI.Layouts.North),
                         76    =>
                           (Swap_Tiles_Command, Concorde.UI.Layouts.East),
                         74    =>
                           (Swap_Tiles_Command, Concorde.UI.Layouts.South),
                         67     =>
                           (Command => Delete_Tile_Command),
                         others => <>);

   ---------------
   -- Add_Child --
   ---------------

   overriding procedure Add_Child
     (Dashboard : in out Root_Dashboard_View;
      Child     : not null access Root_View_Type'Class)
   is
      Cell : constant Cell_View_Type := new Root_Cell_View_Type;
   begin
      Cell.Create (Dashboard.Session, Dashboard.Body_Div.all, "");
      Cell.Cell_Contents := View_Type (Child);
      Cell.Cell_Contents.Create (Dashboard.Session, Cell.Gnoga_View.all, "");
      Cell.Cell_Contents.Gnoga_View.Add_Class ("dashboard-cell-contents");
      Cell.Tile := Dashboard.Model.New_Tile;
      Dashboard.Cells.Append (Cell);
      Dashboard.Set_Active_Cell (Cell);
      Dashboard.Resize;
   end Add_Child;

   ----------
   -- Cell --
   ----------

   function Cell
     (View  : Root_Dashboard_View'Class;
      Index : Concorde.UI.Layouts.Tile_Index)
      return Cell_View_Type
   is
      use type Concorde.UI.Layouts.Tile_Index;
   begin
      for Cell of View.Cells loop
         if Cell.Tile = Index then
            return Cell;
         end if;
      end loop;
      return null;
   end Cell;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (View : in out Root_Dashboard_View)
   is
      procedure Close_Cell
        (Index  : Concorde.UI.Layouts.Tile_Index;
         Layout : Concorde.UI.Layouts.Layout_Tile);

      ----------------
      -- Close_Cell --
      ----------------

      procedure Close_Cell
        (Index  : Concorde.UI.Layouts.Tile_Index;
         Layout : Concorde.UI.Layouts.Layout_Tile)
      is
         pragma Unreferenced (Layout);
         Cell : Cell_View_Type :=
                  View.Cell (Index);
      begin
         Destroy (View_Type (Cell));
      end Close_Cell;

   begin
      View.Top_Toolbar.Close;
      View.Model.Layout.Iterate (Close_Cell'Access);
   end Close;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (View : in out Root_Cell_View_Type)
   is
   begin
      Destroy (View.Cell_Contents);
   end Close;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View    : not null access Root_Dashboard_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String)
   is
      Gnoga_View : constant Dashboard_Gnoga_View_Access :=
                     new Dashboard_Gnoga_View;
   begin
      Gnoga_View.Create
        (Parent      => Parent,
         ID          => Id);

      Gnoga_View.Dashboard := Dashboard_Access (View);
      View.Create_With_Gnoga_View (Session, Gnoga_View);
      Create_Toolbar (View, Gnoga_View);
      Create_Layout (View, Gnoga_View);
   end Create;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View    : not null access Root_Cell_View_Type;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String)
   is
      Gnoga_View : constant Cell_Gnoga_View_Access :=
                     new Cell_Gnoga_View;
   begin
      Gnoga_View.Create (Parent, Id);
      Gnoga_View.Class_Name ("dashboard-cell-container");
      Gnoga_View.Cell := Cell_View_Type (View);
      View.Create_With_Gnoga_View (Session, Gnoga_View);
   end Create;

   -------------------
   -- Create_Layout --
   -------------------

   procedure Create_Layout
     (View       : not null access Root_Dashboard_View'Class;
      Gnoga_View : Dashboard_Gnoga_View_Access)
   is
   begin
      View.Body_Div := new Gnoga.Gui.Element.Common.DIV_Type;
      View.Body_Div.Create (Gnoga_View.all);
      View.Body_Div.Class_Name ("dashboard-body");
      Gnoga_View.Fill_Dock (View.Body_Div);
   end Create_Layout;

   --------------------
   -- Create_Toolbar --
   --------------------

   procedure Create_Toolbar
     (View       : not null access Root_Dashboard_View'Class;
      Gnoga_View : Dashboard_Gnoga_View_Access)
   is
      use Concorde.UI.Models.Commands;
      use Concorde.UI.Models.Commands.Execute;
      use Concorde.UI.Models.Toolbar;
      use Concorde.UI.Views.Toolbar;
      Model : constant Toolbar_Model := Create_Toolbar_Model;
      Exec_Command : constant Command_Type := Execute_Command_Line;
--        Faction_Name : constant Toolbar_Item :=
--                         Text_Label_Item
--                           (Concorde.Factions.Name
--                              (View.Session.Faction));
--        Exec_Item    : constant Toolbar_Item :=
--                         Text_Entry_Item (Exec_Command);
      Stop_Item           : constant Toolbar_Item :=
                              Localised_Button_Item
                                (Session         => View.Session,
                                 Key             => "stop-server",
                                 Command         => Exec_Command,
                                 Activation_Text => "stop-server");
      Pause_Resume_Item  : constant Toolbar_Item :=
                              Toggle_Button_Item
                                (Session       => View.Session,
                                 Base_Class    => "playpause",
                                 False_Class   => "pause",
                                 True_Class    => "play",
                                 False_Command => "resume",
                                 True_Command  => "pause",
                                 Start_State   => True);
      Clock_Item   : constant Toolbar_Item :=
                              Clock.Create (View.Session);
      Command_Item        : constant Toolbar_Item :=
                              Text_Entry_Item
                                (Session => View.Session,
                                 Command => Exec_Command);
   begin
      View.Top_Toolbar :=
        Toolbar_View (Model, (Pause_Resume_Item,
                      Stop_Item, Clock_Item, Command_Item));
      View.Top_Toolbar.Create (View.Session, Gnoga_View.all, "top-toolbar");
      Gnoga_View.Top_Dock (View.Top_Toolbar.Gnoga_View);
   end Create_Toolbar;

   --------------------
   -- Dashboard_View --
   --------------------

   function Dashboard_View
     (Model : not null access
        Concorde.UI.Models.Dashboard.Root_Dashboard_Model'Class)
      return View_Type
   is
      View    : constant Dashboard_Access := new Root_Dashboard_View;
   begin
      View.Set_Model (Model);
      return View_Type (View);
   end Dashboard_View;

   ----------------------
   -- Handle_Key_Press --
   ----------------------

   overriding procedure Handle_Key_Press
     (View    : in out Root_Dashboard_View;
      Meta    : Boolean;
      Alt     : Boolean;
      Control : Boolean;
      Shift   : Boolean;
      Key     : Natural)
   is
      pragma Unreferenced (Meta, Alt, Control, Shift);
   begin
      if Key in Command_Bindings'Range then
         declare
            Binding : constant Dashboard_Command :=
                        Command_Bindings (Key);
         begin
            case Binding.Command is
               when No_Command =>
                  null;
               when Move_Focus_Command | Swap_Tiles_Command =>
                  if View.Active_Cell /= null then
                     declare
                        use Concorde.UI.Layouts;
                        New_Index : constant Tile_Count :=
                                      View.Model.Layout.Get_Neighbour_Tile
                                        (View.Active_Cell.Tile,
                                         Binding.Direction);
                     begin
                        if New_Index /= 0 then
                           if Binding.Command = Move_Focus_Command then
                              declare
                                 Cell : constant Cell_View_Type :=
                                          View.Cell (New_Index);
                              begin
                                 if Cell /= null then
                                    View.Set_Active_Cell (Cell);
                                 end if;
                              end;
                           else
                              View.Model.Swap_Tiles
                                (View.Active_Cell.Tile,
                                 New_Index);
                              View.Resize;
                           end if;
                        end if;
                     end;
                  end if;
               when Delete_Tile_Command =>
                  if View.Active_Cell /= null then
                     declare
                        use Concorde.UI.Layouts;
                        New_Active_Tile : Tile_Count;
                     begin
                        View.Model.Delete_Tile (View.Active_Cell.Tile,
                                                New_Active_Tile);
                        Destroy (View_Type (View.Active_Cell));
                        if New_Active_Tile = No_Tile then
                           View.Active_Cell := null;
                        else
                           View.Set_Active_Cell
                             (View.Cell (New_Active_Tile));
                        end if;
                        View.Resize;
                     end;
                  end if;
            end case;
         end;
      end if;

   end Handle_Key_Press;

   -------------------
   -- Model_Changed --
   -------------------

   overriding procedure Model_Changed
     (View : in out Root_Dashboard_View)
   is
   begin
      View.Resize;
   end Model_Changed;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (View : in out Root_Dashboard_View)
   is
   begin
      null;
   end Render;

   ------------
   -- Resize --
   ------------

   overriding procedure Resize
     (View : in out Root_Cell_View_Type)
   is
   begin
      View.Cell_Contents.Gnoga_View.Width
        (View.Gnoga_View.Width);
      View.Cell_Contents.Gnoga_View.Height
        (View.Gnoga_View.Height - 24);
      View.Cell_Contents.Resize;
   end Resize;

   ------------
   -- Resize --
   ------------

   overriding procedure Resize
     (View : in out Root_Dashboard_View)
   is

      Dashboard_Width : constant Non_Negative_Real :=
                          Non_Negative_Real
                            (View.Gnoga_View.Width - 16);
      Dashboard_Height : constant Non_Negative_Real :=
                           Non_Negative_Real
                             (View.Gnoga_View.Height - 64);

      function To_Across (X : Unit_Real) return Natural
      is (Natural (X * Dashboard_Width));

      function To_Down (Y : Unit_Real) return Natural
      is (Natural (Y * Dashboard_Height));

      procedure Resize_Cell
        (Index  : Concorde.UI.Layouts.Tile_Index;
         Layout : Concorde.UI.Layouts.Layout_Tile);

      -----------------
      -- Resize_Cell --
      -----------------

      procedure Resize_Cell
        (Index  : Concorde.UI.Layouts.Tile_Index;
         Layout : Concorde.UI.Layouts.Layout_Tile)
      is
      begin
         View.Cell (Index).Set_Layout
           (Left     => To_Across (Layout.X),
            Top      => To_Down (Layout.Y),
            Width    => To_Across (Layout.Width),
            Height   => To_Down (Layout.Height));
      end Resize_Cell;

   begin
      View.Model.Layout.Iterate (Resize_Cell'Access);
   end Resize;

   ---------------------
   -- Set_Active_Cell --
   ---------------------

   procedure Set_Active_Cell
     (Dashboard : in out Root_Dashboard_View'Class;
      Cell      : Cell_View_Type)
   is
   begin
      if Dashboard.Active_Cell /= null then
         Dashboard.Active_Cell.Cell_Contents.Gnoga_View.Remove_Class
           ("dashboard-active-cell");
      end if;
      Cell.Cell_Contents.Gnoga_View.Add_Class ("dashboard-active-cell");
      Dashboard.Active_Cell := Cell;
   end Set_Active_Cell;

   ----------------
   -- Set_Layout --
   ----------------

   procedure Set_Layout
     (View          : not null access Root_Cell_View_Type'Class;
      Left, Top     : Natural;
      Width, Height : Natural)
   is
      use Gnoga.Gui.Element;
   begin
      View.Gnoga_View.Position (Absolute);
      View.Gnoga_View.Left (Left);
      View.Gnoga_View.Top (Top);
      View.Gnoga_View.Width (Width);
      View.Gnoga_View.Height (Height);

      View.Resize;

   end Set_Layout;

end Concorde.UI.Views.Dashboard;
