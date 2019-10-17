with Concorde.Color;
with Concorde.Signals;

with Concorde.Elementary_Functions;

with Concorde.Factions;
with Concorde.Ships.Lists;
with Concorde.Terrain;
with Concorde.Worlds;

with Concorde.UI.Views.Model_Views;
with Concorde.UI.Views.Picture;

with Concorde.Db;

package body Concorde.UI.Views.World is

   Orbiting_Ship_Resource_Name : constant String := "orbiting-ship";

   package Base_View is
     new Concorde.UI.Views.Model_Views
       (Base_View_Type  =>
           Concorde.UI.Views.Picture.Root_Picture_View,
        View_Model_Type =>
           Concorde.UI.Models.World.Root_World_Model);

   type Root_World_View is
     new Base_View.View_Type with
      record
         View_Radius      : Non_Negative_Real := 1.5;
         Clock_Handler_Id : Concorde.Signals.Handler_Id;
      end record;

   type World_Access is access all Root_World_View'Class;

   overriding procedure Create
     (View    : not null access Root_World_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String);

   overriding procedure Close
     (View : in out Root_World_View);

   overriding procedure On_Mouse_Click
     (View : in out Root_World_View;
      X, Y : Real);

   overriding procedure Draw_Picture
     (View  : in out Root_World_View;
      Layer : Concorde.UI.Views.Picture.Layer_Index);

   type World_Gnoga_View is
     new Gnoga.Gui.View.View_Type with
      record
         World : World_Access;
      end record;

   type World_Gnoga_View_Access is
     access all World_Gnoga_View'Class;

   type World_Signal_Data is
     new Concorde.Signals.Signal_Data_Interface with
      record
         View : World_Gnoga_View_Access;
      end record;

   procedure Handle_Clock_Tick
     (Object : Concorde.Signals.Signaler'Class;
      Data   : Concorde.Signals.Signal_Data_Interface'Class);

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (View : in out Root_World_View)
   is
   begin
      View.Session.Remove_Handler
        (Concorde.Sessions.Signal_Clock_Tick,
         View.Clock_Handler_Id);
      View.Gnoga_View.Remove;
      Concorde.UI.Views.Picture.Root_Picture_View (View).Close;
   end Close;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View    : not null access Root_World_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String)
   is
      use Concorde.UI.Views.Picture;

      Gnoga_View   : constant World_Gnoga_View_Access :=
                       new World_Gnoga_View;
      Picture_View : constant access Root_Picture_View :=
                       Root_Picture_View (View.all)'Access;
   begin
      Gnoga_View.Create (Parent, Id);
      Gnoga_View.World := World_Access (View);
      View.Create_With_Gnoga_View (Session, Gnoga_View);

      Picture_View.Create_Picture_View
        (Gnoga_View => Gnoga_View.all,
         Layers     => 2);

      View.Set_Viewport
        (-View.View_Radius,
         -View.View_Radius,
         View.View_Radius * 2.0,
         View.View_Radius * 2.0);

      declare
         Data : constant World_Signal_Data :=
                  (View => Gnoga_View);
      begin
         View.Clock_Handler_Id :=
           Session.Add_Handler
             (Signal  => Concorde.Sessions.Signal_Clock_Tick,
              Handler => Handle_Clock_Tick'Access,
              Data    => Data);
      end;

      Picture_View.Add_Image_Resource
        (Name => Orbiting_Ship_Resource_Name,
         Path => "img/icons/spaceship-icon.png");

      View.Queue_Render;

   end Create;

   --------------------
   -- Create_Picture --
   --------------------

   overriding procedure Draw_Picture
     (View  : in out Root_World_View;
      Layer : Concorde.UI.Views.Picture.Layer_Index)
   is
      Radius : constant Non_Negative_Real :=
                 Concorde.Worlds.Radius
                   (View.Model.World);

      Ships : Concorde.Ships.Lists.List;

   begin
      View.Clear;
      View.Font ("OpenSans", 10.0);

      case Layer is
         when 1 =>
            declare
               procedure Draw_Sector
                 (Sector : Concorde.Db.World_Sector_Reference);

               -----------------
               -- Draw_Sector --
               -----------------

               procedure Draw_Sector
                 (Sector : Concorde.Db.World_Sector_Reference)
               is
               begin
                  if Concorde.Worlds.Get_Centre (Sector).Y > 0.0 then
                     declare
                        use type Concorde.Db.Faction_Reference;
                        Border : constant Concorde.Worlds.Sector_Vertex_Array :=
                                   Concorde.Worlds.Get_Vertices (Sector);
                        Polygon : Concorde.UI.Views.Picture.Point_Array
                          (Border'Range);
                        Owner   : constant Concorde.Db.Faction_Reference :=
                                    Concorde.Worlds.Get_Owner (Sector);
                     begin
                        for I in Border'Range loop
                           Polygon (I) := (Border (I).X, Border (I).Z);
                        end loop;

                        if Owner = Concorde.Db.Null_Faction_Reference then
                           View.Fill_Color
                             (Concorde.Terrain.Color
                                (Concorde.Worlds.Get_Terrain
                                     (Sector)));
                        else
                           View.Fill_Color
                             (Concorde.Factions.Color
                                (Concorde.Factions.Get (Owner)));
                        end if;
                        View.Polygon (Polygon, Filled => True);
                     end;
                  end if;
               end Draw_Sector;

            begin
               View.Draw_Color (Concorde.Color.White);
               Concorde.Worlds.Scan_Surface
                 (View.Model.World, Draw_Sector'Access);
            end;

         when 2 =>
            Concorde.Worlds.Get_Ships (View.Model.World, Ships);

            for Ship of Ships loop
               declare
                  use Concorde.Elementary_Functions;
                  Orbit       : constant Non_Negative_Real :=
                                  Ship.Orbit / Radius;
                  Longitude   : constant Non_Negative_Real :=
                                  Ship.Current_Longitude;
                  Inclination : constant Real := Ship.Inclination;
                  X           : constant Real :=
                                  Orbit * Cos (Longitude, 360.0)
                                  * Cos (Inclination, 360.0);
                  Y           : constant Real :=
                                  Orbit * Cos (Longitude, 360.0)
                                  * Sin (Inclination, 360.0);
                  Z           : constant Real :=
                                  Orbit * Sin (Longitude, 360.0);

               begin
                  if X not in -1.0 .. 1.0 or else Z > 0.0 then
                     View.Image
                       (Orbiting_Ship_Resource_Name, (X, Y), 32, 32);
                  end if;
               end;
            end loop;

         when others =>
            null;
      end case;
   end Draw_Picture;

   -----------------------
   -- Handle_Clock_Tick --
   -----------------------

   procedure Handle_Clock_Tick
     (Object : Concorde.Signals.Signaler'Class;
      Data   : Concorde.Signals.Signal_Data_Interface'Class)
   is
      pragma Unreferenced (Object);
   begin
      World_Signal_Data (Data).View.World.Queue_Render_Layer (2);
   end Handle_Clock_Tick;

   --------------------
   -- On_Mouse_Click --
   --------------------

   overriding procedure On_Mouse_Click
     (View : in out Root_World_View;
      X, Y : Real)
   is null;

   ----------------------
   -- World_View --
   ----------------------

   function World_View
     (Model : not null access
        Concorde.UI.Models.World.Root_World_Model'Class)
      return View_Type
   is
      View : constant World_Access := new Root_World_View;
   begin
      View.Set_Model (Model);
      return View_Type (View);
   end World_View;

end Concorde.UI.Views.World;
