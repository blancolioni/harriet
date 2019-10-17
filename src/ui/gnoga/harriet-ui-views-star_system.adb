with Ada.Strings.Fixed;

with Concorde.Commands;
with Concorde.Signals;

with Concorde.Elementary_Functions;
--  with Concorde.Solar_System;

with Concorde.Climates;
with Concorde.Factions;
with Concorde.Ships.Lists;
with Concorde.Star_Systems;
with Concorde.Terrain;

with Concorde.UI.Views.Model_Views;
with Concorde.UI.Views.Picture;

with Concorde.Db;

package body Concorde.UI.Views.Star_System is

   package Base_View is
     new Concorde.UI.Views.Model_Views
       (Base_View_Type  =>
           Concorde.UI.Views.Picture.Root_Picture_View,
        View_Model_Type =>
           Concorde.UI.Models.Star_System.Root_Star_System_Model);

   type Root_Star_System_View is
     new Base_View.View_Type with
      record
         View_Radius      : Non_Negative_Real := 10.0;
         Clock_Handler_Id : Concorde.Signals.Handler_Id;
      end record;

   type Star_System_Access is access all Root_Star_System_View'Class;

   overriding procedure Create
     (View    : not null access Root_Star_System_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String);

   overriding procedure On_Mouse_Click
     (View : in out Root_Star_System_View;
      X, Y : Real);

   overriding procedure Close
     (View : in out Root_Star_System_View);

   overriding procedure Draw_Picture
     (View : in out Root_Star_System_View;
      Layer : Concorde.UI.Views.Picture.Layer_Index);

   type Star_System_Gnoga_View is
     new Gnoga.Gui.View.View_Type with
      record
         Star_System : Star_System_Access;
      end record;

   type Star_System_Gnoga_View_Access is
     access all Star_System_Gnoga_View'Class;

   type Star_System_Signal_Data is
     new Concorde.Signals.Signal_Data_Interface with
      record
         View : Star_System_Gnoga_View_Access;
      end record;

   procedure Handle_Clock_Tick
     (Object : Concorde.Signals.Signaler'Class;
      Data   : Concorde.Signals.Signal_Data_Interface'Class);

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (View : in out Root_Star_System_View)
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
     (View    : not null access Root_Star_System_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String)
   is
      use Concorde.UI.Views.Picture;

      Gnoga_View   : constant Star_System_Gnoga_View_Access :=
                       new Star_System_Gnoga_View;
      Picture_View : constant access Root_Picture_View :=
                       Root_Picture_View (View.all)'Access;
   begin
      Gnoga_View.Create (Parent, Id);
      Gnoga_View.Star_System := Star_System_Access (View);
      View.Create_With_Gnoga_View (Session, Gnoga_View);

      Picture_View.Create_Picture_View (Gnoga_View.all);

      View.Set_Viewport
        (-View.View_Radius,
         -View.View_Radius,
         View.View_Radius * 2.0,
         View.View_Radius * 2.0);

      declare
         Data : constant Star_System_Signal_Data :=
                  (View => Gnoga_View);
      begin
         View.Clock_Handler_Id :=
           Session.Add_Handler
             (Signal  => Concorde.Sessions.Signal_Clock_Tick,
              Handler => Handle_Clock_Tick'Access,
              Data    => Data);
      end;

   end Create;

   ------------------
   -- Draw_Picture --
   ------------------

   overriding procedure Draw_Picture
     (View  : in out Root_Star_System_View;
      Layer : Concorde.UI.Views.Picture.Layer_Index)
   is
      pragma Unreferenced (Layer);

      Ships : Concorde.Ships.Lists.List;

--        procedure Set_World_Color
--          (Category : Concorde.Db.World_Category;
--           Climate  : Concorde.Db.Climate_Category);
--
--        ---------------------
--        -- Set_World_Color --
--        ---------------------
--
--        procedure Set_World_Color
--          (Category : Concorde.Db.World_Category;
--           Climate  : Concorde.Db.Climate_Category)
--        is
--           use all type Concorde.Db.World_Category;
--           use all type Concorde.Db.Climate_Category;
--           Color : Concorde.Color.Concorde_Color := Concorde.Color.White;
--        begin
--           case Category is
--              when Asteroid =>
--                 Color := (0.6, 0.6, 0.6, 1.0);
--              when Dwarf | Terrestrial | Super_Terrestrial =>
--                 case Climate is
--                    when Temperate =>
--                       Color := (0.0, 0.6, 0.0, 1.0);
--                    when Desert =>
--                       Color := Concorde.Color.From_String ("#e68e0d");
--                    when Water =>
--                       Color := Concorde.Color.From_String ("#0d8ee0");
--                    when Iceball =>
--                       Color := Concorde.Color.White;
--                    when others =>
--                       Color := (1.0, 0.0, 0.0, 1.0);
--                 end case;
--              when Sub_Jovian =>
--                 Color := Concorde.Color.From_String ("#3f54ba");
--              when Jovian =>
--                 Color := Concorde.Color.From_String ("#ceb8b8");
--              when Super_Jovian =>
--                 Color := Concorde.Color.From_String ("#d8ca9d");
--           end case;
--           View.Fill_Color (Color);
--        end Set_World_Color;

   begin
      View.Clear;
      View.Font ("OpenSans", 10.0);

      Concorde.Star_Systems.Get_Ships
        (Concorde.Star_Systems.Get (View.Model.Star_System),
         Ships);

      for I in 1 .. View.Model.World_Count loop
         declare
            use Concorde.Elementary_Functions;
            Reference : constant Concorde.Db.World_Reference :=
                          View.Model.Reference (I);
            Orbit : constant Non_Negative_Real :=
                      View.Model.Orbit (I);
            Longitude : constant Non_Negative_Real :=
                          View.Model.Current_Longitude (I);
            X         : constant Real :=
                          Orbit * Cos (Longitude, 360.0);
            Y         : constant Real :=
                          Orbit * Sin (Longitude, 360.0);
            Radius    : constant Non_Negative_Real :=
                          View.Model.Radius (I);
         begin
            View.Draw_Color ((0.2, 0.3, 0.4, 0.6));
            View.Circle ((0.0, 0.0), Orbit, False);
            View.Fill_Color
              (Concorde.Terrain.Color
                 (Concorde.Climates.Default_Terrain
                      (View.Model.Climate (I))));
            View.Circle ((X, Y), Radius / 10.0, True);

            declare
               use Concorde.Db;
               Count : Natural := 0;
            begin
               for Ship of Ships loop
                  if Ship.World = Reference then
                     View.Fill_Color
                       (Concorde.Factions.Get (Ship.Owner).Color);
                     View.Label
                       ((X, Y), 10, 20 + Count * 12, Ship.Name);
                     Count := Count + 1;
                  end if;
               end loop;
            end;

         end;
      end loop;

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
      Star_System_Signal_Data (Data).View.Star_System.Queue_Render;
   end Handle_Clock_Tick;

   --------------------
   -- On_Mouse_Click --
   --------------------

   overriding procedure On_Mouse_Click
     (View : in out Root_Star_System_View;
      X, Y : Real)
   is
      use Concorde.Elementary_Functions;
      Orbit       : constant Non_Negative_Real :=
                      Sqrt (X ** 2 + Y ** 2);
      Longitude   : constant Real :=
                      Arctan (Y, X, 360.0);
      World_Index : constant Natural :=
                      View.Model.Find_World_Index (Orbit, Longitude);
   begin
      if World_Index /= 0 then
         Concorde.Commands.Execute_Command_Line
           ("load-world-view --star-system-name="
            & Concorde.Star_Systems.Name (View.Model.Star_System)
            & " --world-number="
            & Ada.Strings.Fixed.Trim (World_Index'Image, Ada.Strings.Left),
            View.Session,
            Concorde.Commands.Null_Writer);
      end if;
   end On_Mouse_Click;

   ----------------------
   -- Star_System_View --
   ----------------------

   function Star_System_View
     (Model : not null access
        Concorde.UI.Models.Star_System.Root_Star_System_Model'Class)
      return View_Type
   is
      View : constant Star_System_Access := new Root_Star_System_View;
   begin
      View.Set_Model (Model);
      return View_Type (View);
   end Star_System_View;

end Concorde.UI.Views.Star_System;
